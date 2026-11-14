# Deterministic, vintage-aware fixture generation for MacroSignals.
#
# The synthetic world is deliberately explicit about its data-generating
# process: policy rates drive FX carry, bond futures are priced off simulated
# yield curves, macro releases are noisy first prints that are revised twice,
# and asset returns have a small lagged response to latent growth. That last
# feature is what gives a point-in-time macro signal something real to find.
# None of the numbers below are hard-coded results: they are inputs to a seeded
# simulation whose outputs are written to inst/fixtures/.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

anchor_path <- function(dates, anchor_dates, values) {
  stats::approx(
    x = as.numeric(as.Date(anchor_dates)),
    y = values,
    xout = as.numeric(as.Date(dates)),
    rule = 2
  )$y
}

ou_noise <- function(n, rho, sd) {
  x <- numeric(n)
  if (n > 1) {
    for (i in 2:n) {
      x[[i]] <- rho * x[[i - 1]] + stats::rnorm(1, 0, sd)
    }
  }
  x
}

last_business_day <- function(date) {
  date <- as.Date(date)
  while (weekdays(date) %in% c("Saturday", "Sunday")) {
    date <- date - 1
  }
  date
}

lag_pad <- function(x, k, fill = 0) {
  if (k <= 0) return(x)
  c(rep(fill, min(k, length(x))), head(x, length(x) - k))
}

# ---------------------------------------------------------------------------
# Main generator
# ---------------------------------------------------------------------------

#' Generate the MacroSignals synthetic universe.
#'
#' @param seed Integer seed. The default is the committed research seed.
#' @param start First business day of the market panel.
#' @param end Last business day (also the default as-of date).
#' @return A list of data frames: series_meta, instrument_meta, observations,
#'   prices, plus a small `coverage` summary.
generate_universe <- function(seed = 20261114,
                              start = as.Date("2018-11-01"),
                              end = as.Date("2026-11-13")) {
  set.seed(seed)
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- business_days(start, end)
  n <- length(dates)

  # -------------------------------------------------------------------------
  # 1. Sovereign curves and policy rates
  # -------------------------------------------------------------------------
  anchor_dates <- as.Date(c(
    "2018-11-01", "2019-06-30", "2020-03-31", "2020-12-31",
    "2021-06-30", "2021-12-31", "2022-06-30", "2022-12-31",
    "2023-06-30", "2023-12-31", "2024-06-30", "2024-12-31",
    "2025-06-30", "2025-12-31", "2026-06-30", "2026-11-13"
  ))

  y10_anchors <- list(
    US = c(3.15, 1.90, 0.70, 0.90, 1.45, 1.50, 3.00, 3.60,
           3.80, 4.00, 4.40, 4.35, 4.30, 4.15, 4.20, 4.05),
    EA = c(0.45, 0.10, -0.45, -0.55, -0.25, -0.25, 1.40, 2.10,
           2.40, 2.30, 2.60, 2.35, 2.55, 2.70, 2.75, 2.70),
    GB = c(1.55, 1.15, 0.45, 0.25, 0.85, 0.95, 2.40, 3.60,
           3.80, 3.90, 4.10, 4.45, 4.55, 4.55, 4.50, 4.35),
    JP = c(0.10, 0.05, -0.05, 0.02, 0.05, 0.10, 0.20, 0.45,
           0.45, 0.65, 0.95, 1.05, 1.45, 1.55, 1.60, 1.65),
    AU = c(2.65, 1.90, 0.95, 0.95, 1.65, 1.75, 3.20, 3.70,
           3.75, 4.20, 4.35, 4.45, 4.35, 4.45, 4.40, 4.35)
  )

  policy_anchors <- list(
    US = c(2.20, 2.40, 0.25, 0.25, 0.25, 0.25, 1.25, 4.00,
           5.00, 5.25, 5.25, 4.75, 4.25, 3.75, 3.75, 3.50),
    EA = c(-0.40, -0.40, -0.50, -0.50, -0.50, -0.50, -0.25, 2.00,
           3.50, 4.00, 3.75, 3.25, 2.50, 2.25, 2.25, 2.00),
    GB = c(0.75, 0.75, 0.25, 0.10, 0.10, 0.25, 0.75, 3.00,
           4.25, 5.00, 5.25, 4.75, 4.25, 4.00, 3.75, 3.50),
    JP = c(-0.10, -0.10, -0.10, -0.10, -0.10, -0.10, -0.10, -0.10,
           -0.10, -0.10, 0.00, 0.25, 0.50, 0.50, 0.75, 0.75),
    AU = c(1.50, 1.25, 0.50, 0.25, 0.25, 0.25, 0.85, 2.50,
           3.35, 4.10, 4.35, 4.35, 4.10, 3.85, 3.60, 3.50)
  )

  yields <- list()
  for (cc in names(y10_anchors)) {
    base <- anchor_path(dates, anchor_dates, y10_anchors[[cc]])
    yields[[cc]] <- base + ou_noise(n, rho = 0.985, sd = 0.04)
  }

  policy <- list()
  for (cc in names(policy_anchors)) {
    base <- anchor_path(dates, anchor_dates, policy_anchors[[cc]])
    policy[[cc]] <- base
  }
  # Countries without a bond instrument still need a policy rate for FX carry.
  policy$NZ <- policy$US + 0.15
  policy$CA <- policy$US - 0.45
  policy$CH <- pmax(policy$US - 2.50, -0.75)
  policy$SE <- pmax(policy$US - 1.75, -0.50)

  # -------------------------------------------------------------------------
  # 2. Latent growth and macro releases
  # -------------------------------------------------------------------------
  month_starts <- seq(as.Date("2018-01-01"), as.Date("2026-10-01"), by = "month")
  n_month <- length(month_starts)
  growth <- list(
    US = as.numeric(stats::arima.sim(list(ar = 0.62), n = n_month, sd = 1)),
    EA = as.numeric(stats::arima.sim(list(ar = 0.58), n = n_month, sd = 1)),
    JP = as.numeric(stats::arima.sim(list(ar = 0.55), n = n_month, sd = 1))
  )

  # Daily, 30-business-day-lagged latent growth drives asset returns. The lag
  # is the source of genuine point-in-time predictability: data is published
  # before the return response has fully played out.
  daily_growth <- function(g) {
    mid <- month_starts + 14
    approx(as.numeric(mid), g, xout = as.numeric(dates), rule = 2)$y
  }
  g_daily <- lapply(growth, daily_growth)
  g_lag <- lapply(g_daily, lag_pad, k = 30)

  macro_specs <- list(
    list(id = "US_CPI", name = "US CPI (all items)", country = "US",
         unit = "index_2015_100", lag = 12, kind = "cpi",
         base = 250, noise = 0.0007, revisions = 2),
    list(id = "US_PAYROLLS", name = "US nonfarm payroll change", country = "US",
         unit = "thousand_persons", lag = 5, kind = "payrolls",
         base = 170, noise = 22, revisions = 2),
    list(id = "US_PMI", name = "US ISM manufacturing PMI", country = "US",
         unit = "diffusion_index", lag = 2, kind = "pmi",
         base = 52.5, noise = 0.7, revisions = 1),
    list(id = "EA_CPI", name = "Euro area HICP (all items)", country = "EA",
         unit = "index_2015_100", lag = 15, kind = "cpi",
         base = 104, noise = 0.0006, revisions = 2),
    list(id = "EA_PMI", name = "Euro area manufacturing PMI", country = "EA",
         unit = "diffusion_index", lag = 3, kind = "pmi",
         base = 51.5, noise = 0.8, revisions = 1),
    list(id = "JP_CPI", name = "Japan CPI (all items)", country = "JP",
         unit = "index_2015_100", lag = 18, kind = "cpi",
         base = 102, noise = 0.0006, revisions = 2)
  )

  cpi_yoy_anchors <- list(
    US = list(
      d = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01", "2020-06-01",
                    "2021-01-01", "2021-12-01", "2022-06-01", "2023-06-01",
                    "2024-06-01", "2025-06-01", "2026-06-01", "2026-10-01")),
      v = c(2.1, 1.6, 2.5, 0.6, 1.4, 7.0, 9.1, 3.0, 3.0, 2.7, 2.6, 2.5)
    ),
    EA = list(
      d = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01", "2020-09-01",
                    "2021-12-01", "2022-10-01", "2023-06-01", "2024-06-01",
                    "2025-06-01", "2026-10-01")),
      v = c(1.3, 1.4, 1.4, -0.3, 5.0, 10.6, 5.5, 2.5, 2.0, 2.1)
    ),
    JP = list(
      d = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01", "2020-10-01",
                    "2021-12-01", "2022-12-01", "2023-06-01", "2024-06-01",
                    "2025-06-01", "2026-10-01")),
      v = c(1.4, 0.2, 0.7, -0.4, 0.8, 4.0, 3.3, 2.8, 3.3, 2.0)
    )
  )

  # Reference period end: the last business day of each month.
  obs_dates <- as.Date(vapply(month_starts, function(m) {
    as.numeric(last_business_day(seq(m, by = "month", length.out = 2)[[2]] - 1))
  }, numeric(1)), origin = "1970-01-01")

  macro_rows <- list()
  series_meta_rows <- list()

  for (spec in macro_specs) {
    g <- growth[[spec$country]]

    if (spec$kind == "cpi") {
      yoy <- anchor_path(
        obs_dates,
        cpi_yoy_anchors[[spec$country]]$d,
        cpi_yoy_anchors[[spec$country]]$v
      ) + 0.55 * g + stats::rnorm(n_month, 0, 0.10)
      growth_m <- (yoy / 100) / 12 +
        stats::rnorm(n_month, 0, 0.0007)
      true_value <- spec$base * cumprod(1 + growth_m)
      noise_sd <- spec$noise * true_value
    } else if (spec$kind == "pmi") {
      true_value <- spec$base + 2.6 * g + stats::rnorm(n_month, 0, 0.45)
      noise_sd <- rep(spec$noise, n_month)
    } else if (spec$kind == "payrolls") {
      true_value <- spec$base + 90 * g + stats::rnorm(n_month, 0, 35)
      noise_sd <- rep(spec$noise, n_month)
    }

    n_rev <- spec$revisions
    for (i in seq_len(n_month)) {
      v0_date <- next_business_day(obs_dates[[i]] + spec$lag)
      rev_dates <- c(
        v0_date,
        next_business_day(v0_date + 30),
        next_business_day(v0_date + 62)
      )
      first <- true_value[[i]] + stats::rnorm(1, 0, noise_sd[[i]])
      values <- c(
        first,
        0.55 * first + 0.45 * true_value[[i]] +
          stats::rnorm(1, 0, noise_sd[[i]] * 0.35),
        0.80 * first + 0.20 * true_value[[i]] +
          stats::rnorm(1, 0, noise_sd[[i]] * 0.25)
      )

      for (r in 0:n_rev) {
        vd <- rev_dates[[r + 1]]
        if (vd > end) next
        macro_rows[[length(macro_rows) + 1]] <- data.frame(
          series_id = spec$id,
          obs_date = obs_dates[[i]],
          vintage_date = vd,
          value = round(values[[r + 1]], 6),
          revision_no = r,
          stringsAsFactors = FALSE
        )
      }
    }

    series_meta_rows[[length(series_meta_rows) + 1]] <- data.frame(
      series_id = spec$id,
      name = spec$name,
      asset_class = "Macro",
      country = spec$country,
      frequency = "monthly",
      unit = spec$unit,
      source = "synthetic-fixture",
      publish_lag_days = spec$lag,
      stringsAsFactors = FALSE
    )
  }

  observations <- do.call(rbind, macro_rows)
  observations <- observations[order(observations$series_id,
                                     observations$obs_date,
                                     observations$vintage_date), ]
  row.names(observations) <- NULL
  series_meta <- do.call(rbind, series_meta_rows)

  # -------------------------------------------------------------------------
  # 3. Market panels
  # -------------------------------------------------------------------------
  common_usd <- stats::rnorm(n, 0, 0.0024)

  fx_specs <- list(
    list(id = "EURUSD", base = "EA", quote = "US", px0 = 1.14, vol = 0.075,
         beta = -0.65, us_w = -0.5, ea_w = 0.5, jp_w = 0),
    list(id = "GBPUSD", base = "GB", quote = "US", px0 = 1.28, vol = 0.085,
         beta = -0.55, us_w = -0.5, ea_w = 0, jp_w = 0),
    list(id = "AUDUSD", base = "AU", quote = "US", px0 = 0.72, vol = 0.095,
         beta = -0.80, us_w = -0.5, ea_w = 0, jp_w = 0),
    list(id = "NZDUSD", base = "NZ", quote = "US", px0 = 0.67, vol = 0.100,
         beta = -0.80, us_w = -0.5, ea_w = 0, jp_w = 0),
    list(id = "USDJPY", base = "US", quote = "JP", px0 = 112, vol = 0.085,
         beta = 0.85, us_w = 0.5, ea_w = 0, jp_w = -0.5),
    list(id = "USDCAD", base = "US", quote = "CA", px0 = 1.32, vol = 0.065,
         beta = 0.70, us_w = 0.5, ea_w = 0, jp_w = 0),
    list(id = "USDCHF", base = "US", quote = "CH", px0 = 1.00, vol = 0.075,
         beta = 0.60, us_w = 0.5, ea_w = 0, jp_w = 0),
    list(id = "USDSEK", base = "US", quote = "SE", px0 = 9.10, vol = 0.090,
         beta = 0.70, us_w = 0.5, ea_w = 0, jp_w = 0)
  )

  fx_prices <- list()
  fx_meta <- list()
  for (spec in fx_specs) {
    carry_bp <- (policy[[spec$base]] - policy[[spec$quote]]) * 100 +
      ou_noise(n, rho = 0.97, sd = 6)
    carry_annual <- carry_bp / 1e4
    trend <- ou_noise(n, rho = 0.985, sd = 0.00016)
    macro_exposure <- spec$us_w * g_lag$US + spec$ea_w * g_lag$EA +
      spec$jp_w * g_lag$JP
    qret <- -0.35 * carry_annual / 252 +
      spec$beta * common_usd + trend +
      stats::rnorm(n, 0, spec$vol / sqrt(252)) +
      0.00005 * macro_exposure
    px <- spec$px0 * exp(cumsum(qret))
    fx_prices[[spec$id]] <- data.frame(
      instrument_id = spec$id,
      obs_date = dates,
      px_last = round(px, 6),
      carry_bp = round(carry_bp, 4),
      stringsAsFactors = FALSE
    )
    fx_meta[[spec$id]] <- data.frame(
      instrument_id = spec$id,
      name = paste0(spec$base, "/", spec$quote, " spot"),
      asset_class = "FX",
      country = spec$base,
      quote_ccy = spec$quote,
      return_sign = 1L,
      us_weight = spec$us_w,
      ea_weight = spec$ea_w,
      jp_weight = spec$jp_w,
      stringsAsFactors = FALSE
    )
  }

  rate_specs <- list(
    list(id = "UST10Y", ctry = "US", name = "US 10y Treasury future proxy", w = -1),
    list(id = "BUND10Y", ctry = "EA", name = "German 10y Bund future proxy", w = -1),
    list(id = "GILT10Y", ctry = "GB", name = "UK 10y Gilt future proxy", w = 0),
    list(id = "JGB10Y", ctry = "JP", name = "Japan 10y JGB future proxy", w = -1),
    list(id = "ACGB10Y", ctry = "AU", name = "Australia 10y bond future proxy", w = 0)
  )

  rate_prices <- list()
  rate_meta <- list()
  for (spec in rate_specs) {
    y <- yields[[spec$ctry]]
    px <- 100 - 8 * (y - y[[1]])
    carry_bp <- (y - policy[[spec$ctry]]) * 100 + 15
    rate_prices[[spec$id]] <- data.frame(
      instrument_id = spec$id,
      obs_date = dates,
      px_last = round(px, 6),
      carry_bp = round(carry_bp, 4),
      stringsAsFactors = FALSE
    )
    rate_meta[[spec$id]] <- data.frame(
      instrument_id = spec$id,
      name = spec$name,
      asset_class = "Rates",
      country = spec$ctry,
      quote_ccy = spec$ctry,
      return_sign = 1L,
      us_weight = if (spec$ctry == "US") spec$w else 0,
      ea_weight = if (spec$ctry == "EA") spec$w else 0,
      jp_weight = if (spec$ctry == "JP") spec$w else 0,
      stringsAsFactors = FALSE
    )
  }

  eq_specs <- list(
    list(id = "SPX500", ctry = "US", name = "S&P 500 index future proxy",
         px0 = 2750, vol = 0.165, div = 1.8, beta = 1.0, w = 1),
    list(id = "NDX100", ctry = "US", name = "Nasdaq 100 index future proxy",
         px0 = 6800, vol = 0.215, div = 0.8, beta = 1.15, w = 1),
    list(id = "DAX40", ctry = "EA", name = "DAX 40 index future proxy",
         px0 = 11500, vol = 0.180, div = 3.0, beta = 0.9, w = 1)
  )

  eq_prices <- list()
  eq_meta <- list()
  market <- stats::rnorm(n, 0, 0.0062)
  for (spec in eq_specs) {
    carry_bp <- spec$div * 100 - policy[[spec$ctry]] * 100
    trend <- ou_noise(n, rho = 0.988, sd = 0.00035)
    macro_exposure <- if (spec$ctry == "US") g_lag$US else g_lag$EA
    ret <- 0.055 / 252 + spec$beta * market + trend +
      stats::rnorm(n, 0, spec$vol / sqrt(252)) +
      0.00005 * macro_exposure
    px <- spec$px0 * exp(cumsum(ret))
    eq_prices[[spec$id]] <- data.frame(
      instrument_id = spec$id,
      obs_date = dates,
      px_last = round(px, 6),
      carry_bp = round(carry_bp, 4),
      stringsAsFactors = FALSE
    )
    eq_meta[[spec$id]] <- data.frame(
      instrument_id = spec$id,
      name = spec$name,
      asset_class = "Equity",
      country = spec$ctry,
      quote_ccy = spec$ctry,
      return_sign = 1L,
      us_weight = if (spec$ctry == "US") spec$w else 0,
      ea_weight = if (spec$ctry == "EA") spec$w else 0,
      jp_weight = 0,
      stringsAsFactors = FALSE
    )
  }

  prices <- do.call(rbind, c(fx_prices, rate_prices, eq_prices))
  prices <- prices[order(prices$instrument_id, prices$obs_date), ]
  row.names(prices) <- NULL
  instrument_meta <- do.call(rbind, c(fx_meta, rate_meta, eq_meta))
  row.names(instrument_meta) <- NULL

  coverage <- list(
    seed = seed,
    start = as.character(start),
    end = as.character(end),
    business_days = n,
    instruments = nrow(instrument_meta),
    macro_series = nrow(series_meta),
    observation_rows = nrow(observations),
    price_rows = nrow(prices),
    asset_classes = sort(unique(instrument_meta$asset_class))
  )

  list(
    series_meta = series_meta,
    instrument_meta = instrument_meta,
    observations = observations,
    prices = prices,
    coverage = coverage
  )
}

#' Write a generated universe to CSV fixtures.
write_fixtures <- function(universe, dir = "inst/fixtures") {
  ensure_dir(dir)
  readr::write_csv(universe$series_meta, file.path(dir, "series_meta.csv"))
  readr::write_csv(universe$instrument_meta, file.path(dir, "instrument_meta.csv"))
  readr::write_csv(universe$observations, file.path(dir, "observations.csv"))
  readr::write_csv(universe$prices, file.path(dir, "prices.csv"))
  write_json(universe$coverage, file.path(dir, "coverage.json"))
  invisible(dir)
}
