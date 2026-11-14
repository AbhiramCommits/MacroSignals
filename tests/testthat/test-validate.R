test_that("clean fixtures pass all six named checks", {
  h <- ms_new_db()
  on.exit(ms_close_db(h))
  result <- validate_universe(h$con, as.Date("2026-11-13"), load_params(
    file.path(MS_ROOT, "config", "params.yml")
  ))

  expect_true(result$ok)
  expect_equal(result$n_fail, 0)
  expect_equal(nrow(result$summary), length(validation_checks))
  expect_true(all(result$summary$severity == "pass"))
  expect_equal(sum(result$summary$n_flagged), 0)
})

test_that("the broken fixture set fires every named validation check", {
  h <- ms_new_db(file.path(MS_ROOT, "inst", "fixtures", "broken"))
  on.exit(ms_close_db(h))
  result <- validate_universe(h$con, as.Date("2026-11-13"), load_params(
    file.path(MS_ROOT, "config", "params.yml")
  ))

  expect_false(result$ok)
  expect_setequal(unique(result$details$check_name), validation_checks)
  expect_true(any(result$details$check_name == "missing_series" &
                    result$details$series_id == "US_UNRATE"))
  expect_true(any(result$details$check_name == "stale_series" &
                    result$details$series_id == "US_PMI"))
  expect_true(any(result$details$check_name == "gap_in_history" &
                    result$details$series_id == "US_PMI"))
  expect_true(any(result$details$check_name == "constant_series" &
                    result$details$series_id == "US_PMI"))
  expect_true(any(result$details$check_name == "zscore_outlier" &
                    result$details$series_id == "US_PMI"))
  expect_true(any(result$details$check_name == "future_vintage" &
                    result$details$series_id == "US_PMI"))
})

test_that("publish_gate blocks on FAIL and names the blocking series", {
  h <- ms_new_db(file.path(MS_ROOT, "inst", "fixtures", "broken"))
  on.exit(ms_close_db(h))
  result <- validate_universe(h$con, as.Date("2026-11-13"), load_params(
    file.path(MS_ROOT, "config", "params.yml")
  ))

  expect_error(publish_gate(result), "PUBLICATION BLOCKED")
  expect_error(publish_gate(result), "US_PMI")
  expect_true("US_UNRATE" %in% result$blocked_series)
})

test_that("publish_gate returns invisibly on clean data", {
  h <- ms_new_db()
  on.exit(ms_close_db(h))
  result <- validate_universe(h$con, as.Date("2026-11-13"), load_params(
    file.path(MS_ROOT, "config", "params.yml")
  ))
  expect_message(res <- publish_gate(result), "passed")
  expect_true(res)
})
