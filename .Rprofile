# Project-level R settings for MacroSignals.
# The pipeline is deterministic: every stochastic fixture is seeded explicitly.
options(
  stringsAsFactors = FALSE,
  warn = 1,
  repos = c(CRAN = "https://cloud.r-project.org"),
  tidyverse.quiet = TRUE
)
Sys.setenv(TZ = "UTC")

# Keep knitr output quiet and reproducible.
if (requireNamespace("knitr", quietly = TRUE)) {
  knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
}
