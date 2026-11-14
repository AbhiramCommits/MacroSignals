#!/usr/bin/env Rscript
# Build and verify the Excel desk workbook.
# Usage: Rscript scripts/build_excel.R [--results DIR] [--out FILE] [--help]

args <- commandArgs(trailingOnly = TRUE)
if (file.exists("R/utils.R")) source("R/utils.R")
maybe_help(
  args,
  "Usage: Rscript scripts/build_excel.R [--results DIR] [--out FILE] [--db PATH]",
  paste("Builds excel/MacroSignals_Desk.xlsx from results/ and verifies that",
        "openxlsx can read back the expected sheets, named ranges and chart part.",
        "Requires a completed `make research` run.", sep = "\n  ")
)

opts <- parse_args(args)
root <- project_root()
setwd(root)
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

results_dir <- if (!is.null(opts$results)) opts$results else "results"
out_path <- if (!is.null(opts$out)) opts$out else "excel/MacroSignals_Desk.xlsx"
db_path <- if (!is.null(opts$db)) opts$db else "data/macrosignals.sqlite"

if (!file.exists(file.path(results_dir, "metrics.csv"))) {
  stop("Missing results/metrics.csv. Run `make research` first.", call. = FALSE)
}

cat("Building workbook from", results_dir, "...\n")
build_workbook(results_dir, out_path, db_path, "config/params.yml")

# --- Read-back verification -------------------------------------------------
expected_sheets <- c("Dashboard", "Ranked_Signals", "Metrics", "EquityCurve",
                     "Validation", "Params")
expected_ranges <- c("rngRanked", "rngMetrics", "rngStatus")

wb <- openxlsx::loadWorkbook(out_path)
sheets <- openxlsx::getSheetNames(out_path)
missing_sheets <- setdiff(expected_sheets, sheets)
if (length(missing_sheets) > 0) {
  stop("Workbook is missing sheets: ", paste(missing_sheets, collapse = ", "),
       call. = FALSE)
}

regions <- tryCatch(openxlsx::getNamedRegions(wb), error = function(e) NULL)
region_names <- if (is.character(regions)) regions else if (is.data.frame(regions)) {
  regions$name
} else {
  character(0)
}
missing_ranges <- setdiff(expected_ranges, region_names)
if (length(missing_ranges) > 0) {
  stop("Workbook is missing named ranges: ",
       paste(missing_ranges, collapse = ", "), call. = FALSE)
}

zip_list <- utils::unzip(out_path, list = TRUE)
if (!"xl/charts/chart1.xml" %in% zip_list$Name) {
  stop("Native chart part xl/charts/chart1.xml is missing.", call. = FALSE)
}

cat("Workbook:", out_path, "\n")
cat("  size        :", round(file.info(out_path)$size / 1024, 1), "KB\n")
cat("  sheets      :", paste(sheets, collapse = ", "), "\n")
cat("  named ranges:", paste(region_names, collapse = ", "), "\n")
cat("  chart part  : xl/charts/chart1.xml (native line chart)\n")
cat("  zip entries :", nrow(zip_list), "\n")

# A quick content check: the ranked table should have all instruments.
ranked_check <- openxlsx::read.xlsx(out_path, sheet = "Ranked_Signals")
cat("  ranked rows :", nrow(ranked_check), "\n")
if (nrow(ranked_check) < 12) {
  stop("Ranked_Signals read back with fewer than 12 rows.", call. = FALSE)
}
cat("Workbook verification passed.\n")
