# Optional live FRED ingestion. The committed pipeline uses the deterministic
# fixtures; this path is opt-in via MACROSIGNALS_SOURCE=fred and FRED_API_KEY.

fred_url <- "https://api.stlouisfed.org/fred/series/observations"

#' Fetch one FRED series as a tibble.
fetch_fred_series <- function(series_id, api_key = Sys.getenv("FRED_API_KEY"),
                              start = "2018-01-01", end = "2026-11-13") {
  if (!nzchar(api_key)) {
    stop("FRED_API_KEY is not set.", call. = FALSE)
  }
  resp <- httr::GET(
    fred_url,
    query = list(
      series_id = series_id,
      api_key = api_key,
      file_type = "json",
      observation_start = start,
      observation_end = end
    )
  )
  httr::stop_for_status(resp)
  body <- httr::content(resp, as = "parsed", type = "application/json")
  if (is.null(body$observations)) {
    stop("Unexpected FRED response for ", series_id, call. = FALSE)
  }
  raw <- do.call(rbind, lapply(body$observations, function(o) {
    data.frame(
      obs_date = o$date,
      value = suppressWarnings(as.numeric(o$value)),
      stringsAsFactors = FALSE
    )
  }))
  raw <- raw[!is.na(raw$value), ]
  raw$series_id <- series_id
  raw[, c("series_id", "obs_date", "value")]
}

#' Fetch the macro legs of the universe from FRED and shape them like the
#' synthetic vintage store (one vintage per observation, dated at the release).
fetch_fred_universe <- function(series_map, api_key = Sys.getenv("FRED_API_KEY"),
                                start = "2018-01-01", end = "2026-11-13") {
  out <- list()
  for (i in seq_len(nrow(series_map))) {
    row <- series_map[i, ]
    df <- fetch_fred_series(row$source_id, api_key, start, end)
    df$vintage_date <- as.character(
      next_business_day(as.Date(df$obs_date) + row$publish_lag_days)
    )
    df$value <- round(df$value, 6)
    df$revision_no <- 0L
    out[[i]] <- df[, c("series_id", "obs_date", "vintage_date", "value",
                       "revision_no")]
  }
  do.call(rbind, out)
}
