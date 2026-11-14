test_that("cross-sectional z-scores have mean zero and unit standard deviation", {
  x <- c(3, 1, 4, 1, 5, 9, 2, 6)
  z <- zscore_cs(x, winsor_level = 0)
  expect_equal(mean(z), 0, tolerance = 1e-12)
  expect_equal(stats::sd(z), 1, tolerance = 1e-12)
})

test_that("winsorisation caps extreme outliers", {
  x <- c(seq_len(100), 100000)
  raw_z <- (x - mean(x)) / stats::sd(x)
  capped <- zscore_cs(x, winsor_level = 0.05)
  expect_lt(max(abs(capped)), 5)
  expect_gt(max(abs(raw_z)), 5)
})

test_that("rank-based weights are dollar neutral with gross exposure one", {
  w <- target_weights(c(A = -2, B = -0.5, C = 0.4, D = 2.5))
  expect_equal(sum(w), 0, tolerance = 1e-12)
  expect_equal(sum(abs(w)), 1, tolerance = 1e-12)
  expect_true(all(w[c("A", "B")] < 0))
  expect_true(all(w[c("C", "D")] > 0))
})

test_that("constant signals produce zero weights and zero turnover", {
  w <- target_weights(c(A = 1, B = 1, C = 1, D = 1))
  expect_equal(sum(abs(w)), 0)
})

test_that("carry z-scores are standardised at every rebalance date", {
  h <- ms_new_db()
  on.exit(ms_close_db(h))
  prices <- db_query(h$con, "SELECT * FROM prices")
  prices$obs_date <- as.Date(prices$obs_date)
  params <- load_params(file.path(MS_ROOT, "config", "params.yml"))
  dates <- rebalance_dates(prices, 252)
  carry <- signal_carry(prices, params, dates)
  stats <- tapply(carry$zscore, carry$as_of_date, function(z) {
    c(mean = mean(z, na.rm = TRUE), sd = stats::sd(z, na.rm = TRUE))
  })
  means <- vapply(stats, function(x) x[["mean"]], numeric(1))
  sds <- vapply(stats, function(x) x[["sd"]], numeric(1))
  expect_true(all(abs(means) < 1e-8))
  expect_true(all(abs(sds - 1) < 1e-8))
})

test_that("the macro signal cannot see vintages published after the decision date", {
  h <- ms_new_db()
  on.exit(ms_close_db(h))
  params <- load_params(file.path(MS_ROOT, "config", "params.yml"))
  meta <- db_query(h$con, "SELECT * FROM instrument_meta ORDER BY instrument_id")
  decision <- as.Date("2020-06-30")
  dates <- c(as.Date("2020-06-30"))

  before <- signal_macro_surprise(h$con, meta, params, dates, decision)

  # Corrupt every vintage that is invisible at the decision date. A
  # point-in-time signal must not move.
  DBI::dbExecute(
    h$con,
    "UPDATE observations SET value = 999999
      WHERE vintage_date > ? AND series_id = 'US_CPI'",
    params = list(as.character(decision))
  )
  after <- signal_macro_surprise(h$con, meta, params, dates, decision)

  expect_equal(before$zscore, after$zscore)
  expect_equal(before$raw_value, after$raw_value)
})
