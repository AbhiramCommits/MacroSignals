SHELL := /bin/bash
R := Rscript

.PHONY: deps ingest validate research report excel test lint all clean

deps:
	$(R) scripts/install_deps.R

ingest:
	$(R) scripts/ingest.R

validate:
	$(R) scripts/validate.R --out results/validation_summary.csv

research:
	$(R) scripts/run_research.R

report:
	$(R) scripts/render_report.R

excel:
	$(R) scripts/build_excel.R

test:
	$(R) -e 'testthat::test_dir("tests/testthat", reporter = "summary")'

lint:
	$(R) -e 'l <- lintr::lint_dir("R"); if (length(l) > 0) { print(l); quit(status = 1) } else { cat("lintr: clean\n") }'

all: deps ingest validate research report excel

clean:
	rm -f data/*.sqlite
	rm -rf reports/out
	rm -f excel/MacroSignals_Desk.xlsx
	rm -f results/metrics.csv results/equity_curve.csv results/ranked_latest.csv
	rm -f results/cost_sensitivity.csv results/run_summary.json
	rm -f results/run_console.txt results/validation_log.csv
	rm -f results/validation_summary.csv
	@echo "Cleaned database, generated results, report and workbook."
