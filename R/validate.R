# The data-quality publication gate.
#
# Six named checks run against the point-in-time view. Any check with severity
# "fail" blocks publication of signals; warnings and informational rows are
# recorded but do not block.

validation_checks <- c(
  "missing_series", "stale_series", "gap_in_history",
  "constant_series", "zscore_outlier", "future_vintage"
)

validation_config <- function(config = NULL) {
  defaults <- list(
    staleness_business_days = list(monthly = 45L, daily = 5L),
    gap_business_days = list(monthly = 62L, daily = 10L),
    constant_lookback = 24L,
    constant_tol = 1e-10,
    zscore_window = 60L,
    zscore_outlier_k = 6,
    zscore_min_periods = 12L,
    forbidden_future_vintages = TRUE
  )
  if (is.null(config)) return(defaults)
  cfg <- if (!is.null(config$validation)) config$validation else config
  for (nm in names(defaults)) {
    if (is.null(cfg[[nm]])) cfg[[nm]] <- defaults[[nm]]
  }
  cfg
}

validation_row <- function(series_id, check_name, severity, message) {
  data.frame(
    series_id = series_id,
    check_name = check_name,
    severity = severity,
    message = message,
    stringsAsFactors = FALSE
  )
}

check_missing <- function(meta, visible) {
  out <- list()
  for (i in seq_len(nrow(meta))) {
    sid <- meta$series_id[[i]]
    if (!sid %in% visible$series_id) {
      out[[length(out) + 1]] <- validation_row(
        sid, "missing_series", "fail",
        "no observations visible at the as-of date"
      )
    }
  }
  out
}

check_stale <- function(meta, visible, as_of_date, cfg) {
  out <- list()
  for (i in seq_len(nrow(meta))) {
    sid <- meta$series_id[[i]]
    rows <- visible[visible$series_id == sid, ]
    if (nrow(rows) == 0) next
    freq <- meta$frequency[[i]]
    limit <- cfg$staleness_business_days[[freq]]
    if (is.null(limit)) limit <- cfg$staleness_business_days$daily
    last_obs <- max(rows$obs_date)
    age <- business_days_between(last_obs, as_of_date)
    if (age > limit) {
      out[[length(out) + 1]] <- validation_row(
        sid, "stale_series", "fail",
        sprintf("last observation %s is %d business days old (limit %d)",
                format(last_obs, "%Y-%m-%d"), age, as.integer(limit))
      )
    }
  }
  out
}

check_gaps <- function(meta, visible, cfg) {
  out <- list()
  for (i in seq_len(nrow(meta))) {
    sid <- meta$series_id[[i]]
    rows <- visible[visible$series_id == sid, ]
    if (nrow(rows) < 2) next
    freq <- meta$frequency[[i]]
    limit <- cfg$gap_business_days[[freq]]
    if (is.null(limit)) limit <- cfg$gap_business_days$daily
    dates <- sort(rows$obs_date)
    for (j in seq_len(length(dates) - 1)) {
      gap <- business_days_between(dates[[j]], dates[[j + 1]])
      if (gap > limit) {
        out[[length(out) + 1]] <- validation_row(
          sid, "gap_in_history", "fail",
          sprintf("gap of %d business days between %s and %s (limit %d)",
                  gap, format(dates[[j]], "%Y-%m-%d"),
                  format(dates[[j + 1]], "%Y-%m-%d"), as.integer(limit))
        )
      }
    }
  }
  out
}

check_constant <- function(meta, visible, cfg) {
  out <- list()
  lookback <- as.integer(cfg$constant_lookback)
  for (sid in meta$series_id) {
    rows <- visible[visible$series_id == sid, ]
    if (nrow(rows) < 3) next
    rows <- rows[order(rows$obs_date), ]
    vals <- utils::tail(rows$value, lookback)
    if (isTRUE(diff(range(vals, na.rm = TRUE)) <= cfg$constant_tol)) {
      out[[length(out) + 1]] <- validation_row(
        sid, "constant_series", "fail",
        sprintf("last %d visible values are constant at %.6f", length(vals),
                vals[[1]])
      )
    }
  }
  out
}

robust_outliers <- function(dates, values, window, min_periods, k) {
  n <- length(values)
  if (n <= min_periods) return(data.frame())
  flags <- list()
  for (i in (min_periods + 1):n) {
    lo <- max(1, i - window)
    history <- values[lo:(i - 1)]
    centre <- stats::median(history, na.rm = TRUE)
    spread <- stats::mad(history, constant = 1.4826, na.rm = TRUE)
    if (!is.finite(spread) || spread <= 0) {
      spread <- stats::sd(history, na.rm = TRUE)
    }
    if (!is.finite(spread) || spread <= 0) next
    z <- (values[[i]] - centre) / spread
    if (abs(z) > k) {
      flags[[length(flags) + 1]] <- data.frame(
        obs_date = dates[[i]], value = values[[i]], z = z,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(flags) == 0) return(data.frame())
  do.call(rbind, flags)
}

check_outliers <- function(meta, visible, cfg) {
  out <- list()
  for (sid in meta$series_id) {
    rows <- visible[visible$series_id == sid, ]
    rows <- rows[order(rows$obs_date), ]
    if (nrow(rows) < cfg$zscore_min_periods + 1) next
    flags <- robust_outliers(
      rows$obs_date, rows$value,
      window = as.integer(cfg$zscore_window),
      min_periods = as.integer(cfg$zscore_min_periods),
      k = cfg$zscore_outlier_k
    )
    if (nrow(flags) == 0) next
    worst <- flags[which.max(abs(flags$z)), ]
    out[[length(out) + 1]] <- validation_row(
      sid, "zscore_outlier", "fail",
      sprintf("value %.4f on %s is %.2f trailing sigma from its %d-period norm",
              worst$value, format(worst$obs_date, "%Y-%m-%d"), worst$z,
              as.integer(cfg$zscore_window))
    )
  }
  out
}

check_future_vintages <- function(con, as_of_date, cfg) {
  if (!isTRUE(cfg$forbidden_future_vintages)) return(list())
  sql <- "SELECT series_id, COUNT(*) AS n, MAX(vintage_date) AS max_vintage
            FROM observations
           WHERE vintage_date > ?
           GROUP BY series_id
           ORDER BY series_id"
  rows <- db_query(con, sql, list(as.character(as.Date(as_of_date))))
  out <- list()
  for (i in seq_len(nrow(rows))) {
    out[[length(out) + 1]] <- validation_row(
      rows$series_id[[i]], "future_vintage", "fail",
      sprintf("%d vintage(s) dated after the as-of date (latest %s)",
              rows$n[[i]], rows$max_vintage[[i]])
    )
  }
  out
}

#' Run the data-quality gate.
#'
#' @param con SQLite connection.
#' @param as_of_date Decision date.
#' @param config Parameter list (the full params.yml list or its `validation`
#'   element).
#' @param run_id Optional run identifier used in validation_log.
#' @return list(ok, summary, details, run_id, as_of_date, n_fail, n_warn,
#'   n_info, blocked_series). `ok` is TRUE only when no FAIL exists.
validate_universe <- function(con, as_of_date, config = NULL, run_id = NULL) {
  as_of_date <- as.Date(as_of_date)
  cfg <- validation_config(config)

  if (is.null(run_id)) {
    run_id <- paste0("validate_", gsub("-", "", as.character(as_of_date)))
  }

  meta <- db_query(con, "SELECT * FROM series_meta ORDER BY series_id")
  visible <- latest_vintage_table(con, as_of_date)

  raw <- c(
    check_missing(meta, visible),
    check_stale(meta, visible, as_of_date, cfg),
    check_gaps(meta, visible, cfg),
    check_constant(meta, visible, cfg),
    check_outliers(meta, visible, cfg),
    check_future_vintages(con, as_of_date, cfg)
  )

  if (length(raw) == 0) {
    details <- data.frame(
      series_id = character(0), check_name = character(0),
      severity = character(0), message = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    details <- do.call(rbind, raw)
    row.names(details) <- NULL
  }

  # One summary row per named check, whether or not it fired.
  summary_rows <- lapply(validation_checks, function(check) {
    rows <- details[details$check_name == check, ]
    severity <- if (nrow(rows) == 0) {
      "pass"
    } else if (any(rows$severity == "fail")) {
      "fail"
    } else if (any(rows$severity == "warn")) {
      "warn"
    } else {
      "info"
    }
    data.frame(
      check_name = check,
      severity = severity,
      n_flagged = nrow(rows),
      series = if (nrow(rows) == 0) "" else
        paste(sort(unique(rows$series_id)), collapse = ", "),
      stringsAsFactors = FALSE
    )
  })
  summary <- do.call(rbind, summary_rows)

  # Idempotent logging for a re-run of the same deterministic run id.
  DBI::dbExecute(con, "DELETE FROM validation_log WHERE run_id = ?",
                 params = list(run_id))
  if (nrow(details) > 0) {
    log_df <- data.frame(
      run_id = run_id,
      checked_at = checked_at(as_of_date),
      series_id = details$series_id,
      check_name = details$check_name,
      severity = details$severity,
      message = details$message,
      stringsAsFactors = FALSE
    )
    db_write(con, "validation_log", log_df)
  }

  n_fail <- sum(details$severity == "fail")
  n_warn <- sum(details$severity == "warn")
  n_info <- sum(details$severity == "info")

  list(
    ok = n_fail == 0,
    summary = summary,
    details = details,
    run_id = run_id,
    as_of_date = as_of_date,
    n_fail = n_fail,
    n_warn = n_warn,
    n_info = n_info,
    blocked_series = sort(unique(details$series_id[details$severity == "fail"]))
  )
}

#' Refuse to publish when the gate found any FAIL.
publish_gate <- function(validation) {
  if (!is.list(validation) || is.null(validation$ok)) {
    stop("publish_gate() expects the result of validate_universe().",
         call. = FALSE)
  }
  if (isTRUE(validation$ok)) {
    message("Publication gate passed: no FAIL checks at ",
            format(validation$as_of_date, "%Y-%m-%d"), ".")
    return(invisible(TRUE))
  }
  fails <- validation$details[validation$details$severity == "fail", ]
  blocked <- unique(fails[, c("series_id", "check_name")])
  lines <- apply(blocked, 1, function(r) {
    paste0("  - ", r[["series_id"]], ": ", r[["check_name"]])
  })
  stop(
    paste0(
      "PUBLICATION BLOCKED: ", nrow(fails), " failing check(s) across ",
      length(unique(fails$series_id)), " series.\n",
      "Blocking series:\n", paste(lines, collapse = "\n"),
      "\nFix the data or quarantine the series before publishing signals."
    ),
    call. = FALSE
  )
}

#' Convenience wrapper: validate and gate in one call.
validate_and_publish <- function(con, as_of_date, config = NULL, run_id = NULL) {
  result <- validate_universe(con, as_of_date, config, run_id)
  publish_gate(result)
  result
}
