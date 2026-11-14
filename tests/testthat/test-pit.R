test_that("as_of never returns a vintage from after the decision date", {
  h <- ms_new_db()
  on.exit(ms_close_db(h))
  decision <- as.Date("2020-03-20")
  pit <- as_of(h$con, c("US_CPI", "US_PMI", "EA_CPI"), decision)

  expect_gt(nrow(pit), 0)
  expect_true(all(pit$vintage_date <= decision))
  expect_equal(anyDuplicated(pit[, c("series_id", "obs_date")]), 0)
})

test_that("a known revision is invisible before its vintage and visible after", {
  h <- ms_new_db()
  on.exit(ms_close_db(h))

  obs_date <- as.Date("2020-01-31")
  hist <- revision_history(h$con, "US_CPI")
  hist <- hist[hist$obs_date == obs_date, ]
  expect_gte(nrow(hist), 2)

  rev1_date <- hist$vintage_date[hist$revision_no == 1][[1]]
  before <- as_of(h$con, "US_CPI", rev1_date - 1)
  after <- as_of(h$con, "US_CPI", rev1_date)
  b <- before[before$obs_date == obs_date, ]
  a <- after[after$obs_date == obs_date, ]

  expect_equal(b$revision_no, 0L)
  expect_equal(a$revision_no, 1L)
  expect_false(isTRUE(all.equal(b$value, a$value)))
})

test_that("latest_vintage_table agrees with as_of for every series", {
  h <- ms_new_db()
  on.exit(ms_close_db(h))
  ids <- db_query(h$con, "SELECT series_id FROM series_meta ORDER BY series_id")$series_id
  decision <- as.Date("2022-06-30")
  all_pit <- as_of(h$con, ids, decision)
  latest <- latest_vintage_table(h$con, decision)
  expect_equal(nrow(all_pit), nrow(latest))
  expect_equal(all_pit$value, latest$value)
})

test_that("unknown series return an empty, typed point-in-time frame", {
  h <- ms_new_db()
  on.exit(ms_close_db(h))
  pit <- as_of(h$con, c("NOT_A_SERIES"), as.Date("2020-01-01"))
  expect_equal(nrow(pit), 0)
  expect_true(all(c("series_id", "obs_date", "vintage_date", "value") %in%
                    names(pit)))
})
