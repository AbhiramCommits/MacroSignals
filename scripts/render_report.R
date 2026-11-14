#!/usr/bin/env Rscript
# Render the parameterised research report.
# Usage:
#   Rscript scripts/render_report.R [--run-id ID] [--params FILE]
#                                   [--results DIR] [--out FILE] [--help]

args <- commandArgs(trailingOnly = TRUE)
if (file.exists("R/utils.R")) source("R/utils.R")
maybe_help(
  args,
  "Usage: Rscript scripts/render_report.R [--run-id ID] [--params FILE] [--out FILE]",
  paste("Renders reports/research_report.Rmd to reports/out/research_report.html.",
        "If --run-id is omitted it is read from results/run_summary.json.",
        "Requires a completed `make research` run.", sep = "\n  ")
)

opts <- parse_args(args)
root <- project_root()
setwd(root)
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

params_path <- if (!is.null(opts$params)) opts$params else "config/params.yml"
results_dir <- if (!is.null(opts$results)) opts$results else "results"
out_path <- if (!is.null(opts$out)) opts$out else
  "reports/out/research_report.html"
db_path <- if (!is.null(opts$db)) opts$db else "data/macrosignals.sqlite"

summary_file <- file.path(results_dir, "run_summary.json")
if (!file.exists(summary_file)) {
  stop("Missing ", summary_file, ". Run `make research` first.", call. = FALSE)
}
if (!file.exists(db_path)) {
  stop("Missing database ", db_path, ". Run `make ingest` first.", call. = FALSE)
}

summary_text <- paste(readLines(summary_file, warn = FALSE), collapse = "\n")
run_id <- if (!is.null(opts[["run-id"]])) opts[["run-id"]] else {
  m <- regmatches(summary_text,
                  regexpr('"run_id"\\s*:\\s*"[^"]+"', summary_text))
  if (length(m) == 0) stop("Could not read run_id from run_summary.json.",
                           call. = FALSE)
  sub('.*"([^"]+)"$', "\\1", m)
}
as_of <- {
  m <- regmatches(summary_text, regexpr('"as_of"\\s*:\\s*"[^"]+"', summary_text))
  sub('.*"([^"]+)"$', "\\1", m)
}

ensure_dir(dirname(out_path))
message("Rendering report for ", run_id, " (as of ", as_of, ") ...")

rmarkdown::render(
  input = "reports/research_report.Rmd",
  output_file = basename(out_path),
  output_dir = dirname(out_path),
  params = list(
    run_id = run_id,
    params_path = params_path,
    results_dir = results_dir,
    db_path = db_path,
    as_of = as_of
  ),
  envir = new.env(parent = globalenv()),
  knit_root_dir = getwd(),
  quiet = TRUE
)

if (!file.exists(out_path)) {
  stop("Report rendering did not produce ", out_path, call. = FALSE)
}
size_kb <- round(file.info(out_path)$size / 1024, 1)
cat("Rendered", out_path, "(", size_kb, "KB )\n")
