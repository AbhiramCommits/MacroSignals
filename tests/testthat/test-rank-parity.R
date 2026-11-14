test_that("R ranking reference matches every VBA golden case", {
  path <- file.path(MS_ROOT, "tests", "vba", "ranking_cases.csv")
  cases <- readr::read_csv(path, show_col_types = FALSE)
  expect_gt(nrow(cases), 0)

  for (i in seq_len(nrow(cases))) {
    values <- as.numeric(strsplit(cases$values[[i]], "|", fixed = TRUE)[[1]])
    expected_ranks <- as.numeric(strsplit(cases$expected_ranks[[i]], "|",
                                          fixed = TRUE)[[1]])
    expected_top <- as.numeric(strsplit(cases$expected_topn[[i]], "|",
                                         fixed = TRUE)[[1]])

    expect_equal(rank_reference(values), expected_ranks,
                 info = cases$case_id[[i]])
    expect_equal(top_n_reference(values, cases$n[[i]]), expected_top,
                 info = cases$case_id[[i]])
  }
})

test_that("rank reference handles ties with average ranks", {
  expect_equal(rank_reference(c(1, 3, 3, 2)), c(4, 1.5, 1.5, 3))
})

test_that("top_n_reference never returns more values than inputs", {
  expect_equal(top_n_reference(c(1, 2), 10), c(2, 1))
  expect_equal(length(top_n_reference(c(1, 2), 0)), 0)
})
