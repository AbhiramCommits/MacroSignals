#!/usr/bin/env Rscript
# Install the R packages required by MacroSignals.
# Usage: Rscript scripts/install_deps.R [--help]

args <- commandArgs(trailingOnly = TRUE)
if (any(args %in% c("-h", "--help"))) {
  cat("Usage: Rscript scripts/install_deps.R\n\n",
      "Installs the CRAN packages used by the MacroSignals pipeline.\n",
      "Already-installed packages are left untouched. Set the CRAN mirror with\n",
      "options(repos = ...) in ~/.Rprofile if the default is unreachable.\n",
      sep = "")
  quit(status = 0)
}

required <- c(
  "tidyverse", "lubridate", "zoo",
  "DBI", "RSQLite",
  "rmarkdown", "knitr",
  "openxlsx",
  "testthat", "lintr",
  "yaml", "httr"
)

installed <- rownames(utils::installed.packages())
missing <- setdiff(required, installed)

if (length(missing) == 0) {
  cat("All required packages are already installed.\n")
} else {
  cat("Installing:", paste(missing, collapse = ", "), "\n")
  utils::install.packages(missing, dependencies = c("Depends", "Imports", "LinkingTo"))
}

still_missing <- setdiff(required, rownames(utils::installed.packages()))
if (length(still_missing) > 0) {
  stop("Missing packages after install: ", paste(still_missing, collapse = ", "),
       call. = FALSE)
}
cat("Dependency check complete for", length(required), "packages.\n")
