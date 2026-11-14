# Signal construction.
#
# Every signal is produced at month-end rebalance dates. Raw values are
# cross-sectionally winsorised and z-scored; the sign convention is always
# "higher z = more attractive long". The macro signal reads the vintage store
# through as_of(), so a revision that had not been published at the decision
# date can never influence the signal.

zscore_cs <- function(x, winsor_level = 0.05) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x)
  if (sum(ok) < 2) return(out)
  q <- stats::quantile(x[ok], probs = c(winsor_level, 1 - winsor_level),
                       na.rm = TRUE, names = FALSE, type = 7)
  xw <- pmin(pmax(x[ok], q[[1]]), q[[2]])
  spread <- stats::sd(xw)
  if (!is.finite(spread) || spread < 1e-12) {
    out[ok] <- 0
  } else {
    out[ok] <- (xw - mean(xw)) / spread
  }
  out
}

rank_signal <- function(z) {
  out <- rep(NA_integer_, length(z))
  ok <- is.finite(z)
  if (sum(ok) > 0) {
    out[ok] <- as.integer(rank(-z[ok], ties.method = "min"))
  }
  out
}

signal_frame <- function(dates, instruments, raw, z, signal_name) {
  data.frame(
    as_of_date = rep(as.Date(dates), each = length(instruments)),
    instrument_id = rep(instruments, times = length(dates)),
    signal_name = signal_name,
    raw_value = as.numeric(raw),
    zscore = as.numeric(z),
    rank = rank_signal(as.numeric(z)),
    stringsAsFactors = FALSE
  )
}

#' Month-end business dates available in the price panel.
rebalance_dates <- function(prices, min_history = 252) {
  dates <- sort(unique(as.Date(prices$obs_date)))
  if (length(dates) <= min_history) return(as.Date(character(0)))
  usable <- dates[(min_history + 1):length(dates)]
  months <- format(usable, "%Y-%m")
  as.Date(vapply(split(usable, months), max, numeric(1)),
          origin = "1970-01-01")
}

#' Carry signal: trailing average carry in basis points, cross-sectionally
#' z-scored at each rebalance.
signal_carry <- function(prices, params, dates = NULL) {
  prices <- prices[order(prices$instrument_id, prices$obs_date), ]
  if (is.null(dates)) dates <- rebalance_dates(prices)
  dates <- sort(as.Date(dates))
  instruments <- sort(unique(prices$instrument_id))
  smooth_n <- as.integer(params$carry$smoothing %||% 5)

  raw <- matrix(NA_real_, nrow = length(dates), ncol = length(instruments),
                dimnames = list(as.character(dates), instruments))
  z <- raw

  for (i in seq_along(dates)) {
    d <- dates[[i]]
    hist <- prices[prices$obs_date <= d, ]
    history_dates <- sort(unique(hist$obs_date))
    keep <- utils::tail(history_dates, smooth_n)
    recent <- hist[hist$obs_date %in% keep, ]
    avg <- tapply(recent$carry_bp, recent$instrument_id, mean, na.rm = TRUE)
    raw[i, names(avg)] <- as.numeric(avg)
    z[i, ] <- zscore_cs(raw[i, ], params$zscore$winsor_level)
  }

  signal_frame(dates, instruments,
               as.vector(t(raw)), as.vector(t(z)), "carry")
}

#' Total-return index from prices plus the daily carry accrual.
total_return_matrix <- function(prices, instrument_meta = NULL) {
  prices <- prices[order(prices$instrument_id, prices$obs_date), ]
  instruments <- sort(unique(prices$instrument_id))
  dates <- sort(unique(prices$obs_date))
  tri <- matrix(NA_real_, nrow = length(dates), ncol = length(instruments),
                dimnames = list(as.character(dates), instruments))
  for (inst in instruments) {
    rows <- prices[prices$instrument_id == inst, ]
    rows <- rows[order(rows$obs_date), ]
    sign <- 1
    if (!is.null(instrument_meta)) {
      meta_row <- instrument_meta[instrument_meta$instrument_id == inst, ]
      if (nrow(meta_row) == 1) sign <- meta_row$return_sign[[1]]
    }
    n <- nrow(rows)
    ret <- c(0, sign * (rows$px_last[-1] / rows$px_last[-n] - 1)) +
      rows$carry_bp / 1e4 / 252
    ret[[1]] <- 0
    tri[, inst] <- cumprod(1 + ret)
  }
  tri
}

#' Momentum signal: blended 1m / 3m / 12m total-return momentum.
signal_momentum <- function(prices, instrument_meta, params, dates = NULL) {
  tri <- total_return_matrix(prices, instrument_meta)
  all_dates <- as.Date(rownames(tri))
  if (is.null(dates)) dates <- rebalance_dates(prices)
  dates <- sort(as.Date(dates))
  lookbacks <- as.integer(unlist(params$momentum$lookbacks))
  weights <- as.numeric(unlist(params$momentum$weights))
  if (length(lookbacks) != length(weights)) {
    stop("momentum lookbacks and weights must have equal length.", call. = FALSE)
  }
  instruments <- colnames(tri)

  raw <- matrix(NA_real_, nrow = length(dates), ncol = length(instruments),
                dimnames = list(as.character(dates), instruments))
  z <- raw

  for (i in seq_along(dates)) {
    idx <- match(dates[[i]], all_dates)
    if (is.na(idx)) next
    blended <- rep(NA_real_, length(instruments))
    parts <- matrix(NA_real_, nrow = length(lookbacks),
                    ncol = length(instruments))
    valid_lb <- rep(FALSE, length(lookbacks))
    for (j in seq_along(lookbacks)) {
      lb <- lookbacks[[j]]
      if (idx - lb < 1) next
      r <- tri[idx, ] / tri[idx - lb, ] - 1
      ann <- (1 + r)^(252 / lb) - 1
      parts[j, ] <- zscore_cs(ann, params$zscore$winsor_level)
      valid_lb[[j]] <- TRUE
    }
    if (all(valid_lb)) {
      blended <- colSums(parts * weights) / sum(weights)
      raw[i, ] <- blended
      z[i, ] <- zscore_cs(blended, params$zscore$winsor_level)
    }
  }

  signal_frame(dates, instruments,
               as.vector(t(raw)), as.vector(t(z)), "momentum")
}

# Map each macro series to the country factor it measures.
macro_country <- function(series_id) {
  out <- rep(NA_character_, length(series_id))
  out[series_id %in% c("US_CPI", "US_PAYROLLS", "US_PMI")] <- "US"
  out[series_id %in% c("EA_CPI", "EA_PMI")] <- "EA"
  out[series_id %in% c("JP_CPI")] <- "JP"
  out
}

#' Standardised latest release versus its own trailing history.
macro_surprise_one <- function(df, unit, trailing_months, min_history) {
  df <- df[order(df$obs_date), ]
  if (nrow(df) < 2) return(NA_real_)
  values <- df$value
  if (grepl("index", unit, fixed = TRUE)) {
    if (nrow(df) < 13) return(NA_real_)
    x <- values / dplyr::lag(values, 12) - 1
  } else {
    x <- values
  }
  x <- x[is.finite(x)]
  if (length(x) < min_history + 1) return(NA_real_)
  latest <- x[[length(x)]]
  history <- utils::tail(x[-length(x)], trailing_months)
  spread <- stats::sd(history)
  if (!is.finite(spread) || spread < 1e-12) return(NA_real_)
  (latest - mean(history)) / spread
}

#' Macro surprise signal. Uses as_of() for every rebalance date, so the signal
#' can only ever see releases (and vintages) available at that date.
signal_macro_surprise <- function(con, instrument_meta, params, dates,
                                  as_of_date = NULL) {
  dates <- sort(as.Date(dates))
  if (!is.null(as_of_date)) dates <- dates[dates <= as.Date(as_of_date)]
  series_meta <- db_query(con, "SELECT * FROM series_meta ORDER BY series_id")
  if (nrow(series_meta) == 0) stop("No series_meta rows for the macro signal.",
                                   call. = FALSE)
  instruments <- sort(unique(instrument_meta$instrument_id))
  trailing <- as.integer(params$macro_surprise$trailing_months %||% 12)
  min_history <- as.integer(params$macro_surprise$min_history %||% 6)

  raw <- matrix(NA_real_, nrow = length(dates), ncol = length(instruments),
                dimnames = list(as.character(dates), instruments))
  z <- raw

  for (i in seq_along(dates)) {
    d <- dates[[i]]
    pit <- as_of(con, series_meta$series_id, d)
    country_scores <- c(US = NA_real_, EA = NA_real_, JP = NA_real_)
    for (cc in names(country_scores)) {
      ids <- series_meta$series_id[macro_country(series_meta$series_id) == cc]
      scores <- vapply(ids, function(sid) {
        rows <- pit[pit$series_id == sid, ]
        if (nrow(rows) == 0) return(NA_real_)
        unit <- series_meta$unit[series_meta$series_id == sid][[1]]
        macro_surprise_one(rows, unit, trailing, min_history)
      }, numeric(1))
      scores <- scores[is.finite(scores)]
      if (length(scores) > 0) country_scores[[cc]] <- mean(scores)
    }

    for (inst in instruments) {
      m <- instrument_meta[instrument_meta$instrument_id == inst, ]
      if (nrow(m) == 0) next
      exposure <- c(US = m$us_weight[[1]], EA = m$ea_weight[[1]],
                    JP = m$jp_weight[[1]])
      if (all(abs(exposure) < 1e-12)) next
      relevant <- exposure[abs(exposure) > 1e-12]
      if (all(!is.finite(country_scores[names(relevant)]))) next
      value <- sum(relevant * country_scores[names(relevant)], na.rm = TRUE) /
        sum(abs(relevant))
      raw[i, inst] <- value
    }
    z[i, ] <- zscore_cs(raw[i, ], params$zscore$winsor_level)
  }

  signal_frame(dates, instruments,
               as.vector(t(raw)), as.vector(t(z)), "macro_surprise")
}

#' Combine signal z-scores into a ranked composite.
#'
#' @param signals A named list of signal data frames, or a long data frame with
#'   columns as_of_date, instrument_id, signal_name, zscore.
#' @param weights Named numeric vector of composite weights.
combine_signals <- function(signals, weights = c(carry = 0.4, momentum = 0.35,
                                                 macro_surprise = 0.25)) {
  if (is.data.frame(signals)) {
    long <- signals
  } else {
    long <- do.call(rbind, signals)
  }
  long <- long[is.finite(long$zscore), ]
  if (nrow(long) == 0) stop("No finite z-scores to combine.", call. = FALSE)

  wide <- tidyr::pivot_wider(
    long[, c("as_of_date", "instrument_id", "signal_name", "zscore")],
    id_cols = c("as_of_date", "instrument_id"),
    names_from = "signal_name",
    values_from = "zscore"
  )
  available <- intersect(names(weights), names(wide))
  if (length(available) == 0) {
    stop("Composite weights do not match any signal names.", call. = FALSE)
  }
  w <- weights[available]
  mat <- as.matrix(wide[, available, drop = FALSE])
  numer <- rowSums(sweep(mat, 2, w, "*"), na.rm = TRUE)
  denom <- rowSums(sweep(!is.na(mat), 2, w, "*"), na.rm = TRUE)
  raw <- ifelse(denom > 0, numer / denom, NA_real_)

  wide$raw_value <- raw
  wide$zscore <- NA_real_
  for (i in seq_along(unique(wide$as_of_date))) {
    d <- unique(wide$as_of_date)[[i]]
    idx <- wide$as_of_date == d
    wide$zscore[idx] <- zscore_cs(raw[idx])
  }
  wide$signal_name <- "composite"
  wide$rank <- rank_signal(wide$zscore)
  wide <- wide[order(wide$as_of_date, wide$instrument_id), ]
  row.names(wide) <- NULL
  wide[, c("as_of_date", "instrument_id", "signal_name", "raw_value",
           "zscore", "rank")]
}

#' Build all four signals for a run.
build_signals <- function(con, prices, instrument_meta, params, as_of_date) {
  dates <- rebalance_dates(prices, min_history = 252)
  dates <- dates[dates <= as.Date(as_of_date)]
  carry <- signal_carry(prices, params, dates)
  momentum <- signal_momentum(prices, instrument_meta, params, dates)
  macro <- signal_macro_surprise(con, instrument_meta, params, dates, as_of_date)
  composite <- combine_signals(
    list(carry = carry, momentum = momentum, macro_surprise = macro),
    weights = unlist(params$composite_weights)
  )
  list(carry = carry, momentum = momentum, macro_surprise = macro,
       composite = composite, dates = dates)
}
