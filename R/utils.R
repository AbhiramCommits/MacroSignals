# Small shared helpers: paths, parameters, dates and run identifiers.

project_root <- function() {
  root <- tryCatch(normalizePath(getwd(), mustWork = TRUE), error = function(e) getwd())
  if (file.exists(file.path(root, "DESCRIPTION"))) {
    return(root)
  }
  # Walk upwards from the working directory when a script is launched from a
  # sub-directory.
  candidate <- root
  for (i in seq_len(6)) {
    candidate <- dirname(candidate)
    if (file.exists(file.path(candidate, "DESCRIPTION"))) {
      return(candidate)
    }
  }
  stop("Could not locate the MacroSignals project root.", call. = FALSE)
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

load_params <- function(path = "config/params.yml") {
  if (!file.exists(path)) {
    stop("Parameter file not found: ", path, call. = FALSE)
  }
  params <- yaml::read_yaml(path)
  params
}

params_hash <- function(path = "config/params.yml") {
  if (!file.exists(path)) {
    stop("Parameter file not found: ", path, call. = FALSE)
  }
  unname(tools::md5sum(path))
}

make_run_id <- function(as_of, hash) {
  paste0("run_", gsub("-", "", as.character(as_of)), "_", substr(hash, 1, 8))
}

business_days <- function(start, end) {
  if (inherits(start, "character")) start <- as.Date(start)
  if (inherits(end, "character")) end <- as.Date(end)
  dates <- seq(start, end, by = "day")
  dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
}

next_business_day <- function(date) {
  date <- as.Date(date)
  while (weekdays(date) %in% c("Saturday", "Sunday")) {
    date <- date + 1
  }
  date
}

business_days_between <- function(from, to) {
  sum(!weekdays(seq(as.Date(from) + 1, as.Date(to), by = "day")) %in%
        c("Saturday", "Sunday"))
}

# Small null-coalescing helper used by parameter defaults.
`%||%` <- function(x, y) if (is.null(x)) y else x

# Format a number for console and markdown output.
fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

# Emit a small usage block and exit when --help is passed.
maybe_help <- function(args, usage, details = NULL) {
  if (any(args %in% c("-h", "--help"))) {
    cat(usage, "\n", sep = "")
    if (!is.null(details)) cat("\n", details, "\n", sep = "")
    quit(status = 0)
  }
  invisible(FALSE)
}

# Parse --key value pairs into a named list without external dependencies.
parse_args <- function(args) {
  out <- list()
  i <- 1
  while (i <= length(args)) {
    a <- args[[i]]
    if (grepl("^--", a)) {
      key <- sub("^--", "", a)
      if (grepl("=", key, fixed = TRUE)) {
        value <- sub("^[^=]*=", "", key)
        key <- sub("=.*$", "", key)
      } else if (i < length(args) && !grepl("^--", args[[i + 1]])) {
        value <- args[[i + 1]]
        i <- i + 1
      } else {
        value <- TRUE
      }
      out[[key]] <- value
    }
    i <- i + 1
  }
  out
}

# ISO-8601 text for validation log timestamps. Uses the run's as-of date so a
# rebuild is byte-for-byte reproducible rather than wall-clock dependent.
checked_at <- function(as_of_date) {
  paste0(as.character(as_of_date), "T23:59:59Z")
}
