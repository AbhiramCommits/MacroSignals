test_that("costs reduce Sharpe for the same signal", {
  case <- golden_case()
  bt0 <- run_backtest(case$signal, case$prices, list(cost_bp = 0),
                      signal_name = "golden")
  bt5 <- run_backtest(case$signal, case$prices, list(cost_bp = 5),
                      signal_name = "golden")
  m0 <- metrics(bt0, "golden")
  m5 <- metrics(bt5, "golden")
  expect_lte(m5$sharpe[m5$basis == "net"], m0$sharpe[m0$basis == "net"])
  expect_lte(m5$ann_return[m5$basis == "net"],
             m0$ann_return[m0$basis == "net"])
})

test_that("maximum drawdown is computed correctly on a hand-built series", {
  dates <- as.Date("2020-01-01") + 0:4
  pnl <- data.frame(
    date = dates,
    gross_pnl = c(0.10, -0.20, 0.05, 0.10, -0.05),
    net_pnl = c(0.10, -0.20, 0.05, 0.10, -0.05),
    turnover = 0,
    stringsAsFactors = FALSE
  )
  m <- metrics(pnl, "hand", cost_bp = 0)
  net <- m[m$basis == "net", ]
  expect_equal(net$max_dd, -0.2, tolerance = 1e-12)
  expect_equal(net$dd_start, dates[[1]])
  expect_equal(net$dd_end, dates[[2]])
})

test_that("turnover is zero for a constant signal", {
  case <- golden_case()
  sig <- case$signal
  sig$zscore <- 0
  bt <- run_backtest(sig, case$prices, list(cost_bp = 5),
                     signal_name = "golden")
  expect_equal(sum(bt$turnover), 0)
  expect_equal(sum(bt$net_pnl), 0, tolerance = 1e-12)
})

test_that("golden metrics reproduce the committed reference values", {
  golden_path <- file.path(MS_ROOT, "tests", "testthat", "golden", "metrics.csv")
  skip_if_not(file.exists(golden_path), "golden metrics not generated")
  golden <- readr::read_csv(golden_path, show_col_types = FALSE)

  case <- golden_case()
  bt <- run_backtest(case$signal, case$prices, case$params,
                     signal_name = "golden")
  m <- metrics(bt, "golden")
  m <- m[order(m$basis), ]
  golden <- golden[order(golden$basis), ]

  expect_equal(m$basis, golden$basis)
  expect_equal(m$ann_return, golden$ann_return, tolerance = 1e-10)
  expect_equal(m$ann_vol, golden$ann_vol, tolerance = 1e-10)
  expect_equal(m$sharpe, golden$sharpe, tolerance = 1e-10)
  expect_equal(m$max_dd, golden$max_dd, tolerance = 1e-10)
  expect_equal(m$calmar, golden$calmar, tolerance = 1e-10)
  expect_equal(m$hit_rate, golden$hit_rate, tolerance = 1e-10)
  expect_equal(m$ann_turnover, golden$ann_turnover, tolerance = 1e-10)
})
