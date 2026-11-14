test_that("the schema creates every required table and round-trips data", {
  h <- ms_new_db()
  on.exit(ms_close_db(h))

  tables <- DBI::dbListTables(h$con)
  required <- c("series_meta", "observations", "prices", "validation_log",
                "signal_values", "backtest_metrics", "instrument_meta")
  expect_true(all(required %in% tables))

  counts <- db_table_counts(h$con)
  expect_gt(counts$rows[counts$table == "series_meta"], 0)
  expect_gt(counts$rows[counts$table == "observations"], 0)
  expect_gt(counts$rows[counts$table == "prices"], 0)

  one <- db_query(h$con, "SELECT * FROM series_meta WHERE series_id = ?",
                  list("US_CPI"))
  expect_equal(nrow(one), 1)
  expect_equal(one$frequency, "monthly")
})

test_that("primary-key conflicts are rejected", {
  h <- ms_new_db()
  on.exit(ms_close_db(h))
  dup <- data.frame(
    series_id = "US_CPI", name = "duplicate", asset_class = "Macro",
    country = "US", frequency = "monthly", unit = "index_2015_100",
    source = "test", publish_lag_days = 12L, stringsAsFactors = FALSE
  )
  expect_error(db_write(h$con, "series_meta", dup), "UNIQUE|constraint")
})

test_that("unknown tables and columns are rejected before SQL runs", {
  h <- ms_new_db()
  on.exit(ms_close_db(h))
  expect_error(db_write(h$con, "not_a_table", data.frame(x = 1)), "Unknown table")
  expect_error(db_write(h$con, "series_meta", data.frame(not_a_column = "x")),
               "Unknown column")
})

test_that("db_init is idempotent", {
  h <- ms_new_db()
  on.exit(ms_close_db(h))
  expect_silent(db_init(h$con, file.path(MS_ROOT, "sql", "schema.sql")))
  expect_silent(db_init(h$con, file.path(MS_ROOT, "sql", "schema.sql")))
})
