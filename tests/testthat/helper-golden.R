# Deterministic hand-built case shared by the golden-metrics test and the
# generator script (scripts/make_golden.R).

golden_case <- function() {
  dates <- business_days(as.Date("2020-01-01"), as.Date("2021-12-31"))
  instruments <- c("A", "B", "C", "D")
  grid <- expand.grid(obs_date = dates, instrument_id = instruments,
                      stringsAsFactors = FALSE)
  grid <- grid[order(grid$instrument_id, grid$obs_date), ]
  grid$px_last <- NA_real_
  grid$carry_bp <- NA_real_
  n <- length(dates)
  for (j in seq_along(instruments)) {
    idx <- grid$instrument_id == instruments[[j]]
    tt <- seq_len(n)
    ret <- 0.0002 * j + 0.0015 * sin(tt * 0.31 + j) +
      0.0008 * cos(tt * 0.07 + 2 * j)
    grid$px_last[idx] <- 100 * cumprod(1 + ret)
    grid$carry_bp[idx] <- 10 * j
  }

  signal_dates <- dates[61:length(dates)]
  signal_dates <- as.Date(
    vapply(split(signal_dates, format(signal_dates, "%Y-%m")), max, numeric(1)),
    origin = "1970-01-01"
  )
  signal_dates <- signal_dates[signal_dates <= as.Date("2021-12-20")]
  sig <- expand.grid(as_of_date = signal_dates, instrument_id = instruments,
                     stringsAsFactors = FALSE)
  sig <- sig[order(sig$as_of_date, sig$instrument_id), ]
  sig$signal_name <- "golden"
  sig$zscore <- sin(seq_len(nrow(sig)) * 0.7) +
    rep(c(-1, -0.3, 0.4, 1.2), times = length(signal_dates))
  sig$raw_value <- sig$zscore
  sig$rank <- NA_integer_
  row.names(sig) <- NULL

  list(
    prices = grid[, c("instrument_id", "obs_date", "px_last", "carry_bp")],
    signal = sig,
    params = list(cost_bp = 5, rebalance = "monthly")
  )
}
