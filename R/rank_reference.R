# Reference implementation of the desk ranking logic.
# The same golden cases (tests/vba/ranking_cases.csv) are exercised by the VBA
# test macro, so the Excel front end has a verified cross-language reference.

#' Average-rank a numeric vector; rank 1 is the largest value.
rank_reference <- function(x) {
  as.numeric(rank(-x, ties.method = "average"))
}

#' Top-n values in descending order.
top_n_reference <- function(x, n) {
  n <- min(as.integer(n), length(x))
  if (n <= 0) return(numeric(0))
  sort(x, decreasing = TRUE)[seq_len(n)]
}
