# MacroSignals

**Vintage-aware macro signal research that a portfolio manager can refresh from Excel.**

Macro research has a data problem before it has a modelling problem: a CPI print,
a payroll number and a PMI release are revised, and a backtest that uses today's
revised history is look-ahead biased. MacroSignals stores every macro observation
as a *vintage* (observation date + publication date + revision number), reads the
database only through point-in-time queries, and refuses to publish signals when a
data-quality gate finds a FAIL.

The toolkit takes seeded fixture data (or optional live FRED data), builds
cross-sectional carry, momentum and macro-surprise signals, backtests them with
transaction costs, writes reproducible research artefacts, renders an R Markdown
report, and generates an Excel desk workbook with VBA refresh and ranking macros.

## What it produces

| Artefact | Path |
|---|---|
| Vintage-aware SQLite store | SQLite file under `data/` (generated; build artefact) |
| Signal and backtest tables | `signal_values`, `backtest_metrics`, `validation_log` in SQLite |
| Headline and diagnostic metrics | `results/metrics.csv` |
| Daily equity curves and drawdowns | `results/equity_curve.csv` |
| Latest ranked cross-section | `results/ranked_latest.csv` |
| Cost-sensitivity grid | `results/cost_sensitivity.csv` |
| Run manifest | `results/run_summary.json` |
| Validation log (CSV mirror for Excel) | `results/validation_log.csv` |
| Research report | `reports/out/research_report.html` |
| Desk workbook | `excel/MacroSignals_Desk.xlsx` |
| VBA sources | `vba/*.bas` |

## Architecture

```
inst/fixtures/*.csv  (or optional FRED API)
        │
        ▼
 scripts/ingest.R ──► SQLite vintage store (data/)
        │              series_meta, observations (vintage store),
        │              prices, instrument_meta
        ▼
 R/pit.R  as_of()  ── latest vintage visible at the decision date
        │
        ▼
 R/validate.R  six-check publication gate (FAIL blocks signals)
        │
        ▼
 R/signals.R  carry · momentum · macro_surprise · composite
        │
        ▼
 R/backtest.R  rank-based dollar-neutral long/short, costs in bp
        │
        ├──► results/*.csv, results/run_summary.json
        │
        ├──► reports/research_report.Rmd ──► reports/out/research_report.html
        │
        └──► R/excel_export.R ──► excel/MacroSignals_Desk.xlsx
                                      ▲
                                      │
                              vba/*.bas refresh + ranking
```

### R/

| File | Responsibility |
|---|---|
| `R/db.R` | `db_connect()`, `db_init()`, parameterised `db_write()` / `db_query()`, row counts |
| `R/utils.R` | project paths, parameter loading, MD5 params hash, business-day helpers, CLI argument parsing |
| `R/json.R` | dependency-free JSON writer for `run_summary.json` |
| `R/generate_fixtures.R` | seeded simulation of curves, FX, equities, macro releases and revisions |
| `R/fetch_fred.R` | opt-in FRED ingestion through `httr` (`MACROSIGNALS_SOURCE=fred`) |
| `R/ingest_helpers.R` | fixture reading, table writes and scratch-database rebuilds |
| `R/pit.R` | `as_of()`, `latest_vintage_table()`, `revision_history()` |
| `R/validate.R` | `validate_universe()` checks and `publish_gate()` |
| `R/signals.R` | `signal_carry()`, `signal_momentum()`, `signal_macro_surprise()`, `combine_signals()` |
| `R/backtest.R` | `target_weights()`, `run_backtest()`, `metrics()`, `cost_sensitivity()` |
| `R/excel_export.R` | openxlsx workbook builder and native OOXML chart injection |
| `R/rank_reference.R` | reference implementation of the VBA ranking logic |

### scripts/

| File | Responsibility |
|---|---|
| `scripts/install_deps.R` | installs the required CRAN packages |
| `scripts/generate_fixtures.R` | regenerates `inst/fixtures/*.csv` from the seed |
| `scripts/ingest.R` | rebuilds the SQLite store and prints row counts |
| `scripts/validate.R` | runs the gate, prints the summary, exits non-zero on FAIL |
| `scripts/run_research.R` | validate → signals → backtests → results and SQLite |
| `scripts/render_report.R` | renders the parameterised R Markdown report |
| `scripts/build_excel.R` | builds and read-back-verifies the workbook |
| `scripts/make_broken_fixtures.R` | writes the deliberately corrupt fixture copy |
| `scripts/make_golden.R` | recomputes the golden backtest metrics |

All scripts accept `--help`. `make` targets: `deps ingest validate research report
excel test lint all clean`.

### vba/

| File | Responsibility |
|---|---|
| `vba/modRefresh.bas` | `RefreshSignals()` shells `Rscript scripts/run_research.R`, reloads CSVs, timestamps the Dashboard |
| `vba/modLoad.bas` | `LoadCsvToSheet()`, CSV parsing, `GetRunSummary()` |
| `vba/modRank.bas` | `RankSignals()`, `TopN()` with average ranks for ties |
| `vba/modValidation.bas` | `CheckStatus()` reads the Validation sheet and paints the status light |
| `vba/modTests.bas` | `RunVbaTests()` asserts ranking behaviour against the shared golden cases |

## Data

The committed numbers come from the deterministic, seeded fixture path. The seed is
**20261114**; no random draw is unseeded. The optional FRED path is opt-in
(`MACROSIGNALS_SOURCE=fred` plus `FRED_API_KEY`) and is not used for any committed
result.

| Item | Value |
|---|---:|
| Macro series | 6 (`US_CPI`, `US_PAYROLLS`, `US_PMI`, `EA_CPI`, `EA_PMI`, `JP_CPI`) |
| Instruments | 16 (8 G10 FX pairs, 5 10-year government bond futures, 3 equity index futures) |
| Business days | 2,097 |
| Date range | 2018-11-01 to 2026-11-13 |
| Observation-vintage rows | 1,675 |
| Price rows | 33,552 |
| Rebalance dates | 86 |
| Signal rows for four signals | 5,504 |

**Release behaviour.** Each macro observation has a reference period end, a
first-print `vintage_date = obs_date + publish_lag_days`, and one or two revised
vintages roughly 30 and 62 days after the first print. Dates are rolled forward to
the next business day. Publication lags are 2 days (US PMI), 5 (US payrolls),
12 (US CPI), 15 (EA CPI), 3 (EA PMI) and 18 (JP CPI). Vintages dated after the
as-of date are not loaded, so a clean gate has nothing to hide; the broken fixture
deliberately adds a 2027 vintage to exercise the `future_vintage` check.

**Market data.** FX spot paths carry interest-rate differentials from simulated
policy rates, so `carry_bp` is a genuine rate differential. Bond futures are priced
off simulated 10-year curves with duration 8, and equity futures use dividend yield
minus funding as carry. Macro releases are noisy first prints of a latent growth
factor; asset returns have a small lagged response to that factor. That lag is what
gives the point-in-time macro signal something to find.

## How to run

```sh
# 1. Install R 4.x packages (already-installed packages are skipped)
Rscript scripts/install_deps.R

# 2. Full chain from an empty database to report + workbook
make all

# 3. Tests and static checks
make test
make lint

# Individual stages
make ingest      # fixtures -> SQLite
make validate    # six-check publication gate
make research    # signals, backtests, results/
make report      # reports/out/research_report.html
make excel       # excel/MacroSignals_Desk.xlsx
make clean
```

`make all` was measured at **65.9 seconds** from `make clean` on the build machine
(Apple M1, R 4.6.1). A fresh local clone was also verified with
`git clone . /tmp/MS_clone && cd /tmp/MS_clone && make clean && make all`; it
completed successfully.

### The PM workflow in Excel

1. Run `make excel` (or `make all`) to generate `excel/MacroSignals_Desk.xlsx`.
2. Import all five `.bas` modules and save the workbook as `.xlsm` — exact steps
   are in [`excel/README.md`](excel/README.md).
3. Open the `.xlsm`, press **Alt+F8**, choose `RefreshSignals`, and click **Run**.
   The macro refuses to run while the Validation sheet contains FAIL rows, calls
   `Rscript scripts/run_research.R`, reloads the result CSVs, timestamps the
   Dashboard and updates the data-quality light.
4. `RunVbaTests` in the same macro list executes the shared ranking golden cases.

## Results

All figures below are copied from `results/metrics.csv` and
`results/run_summary.json` for run `run_20261113_c9f439e3`, as of 2026-11-13,
with a one-way transaction cost of **5 bp** (net rows). Gross rows are zero-cost.
Max drawdown is peak-to-trough on the net/gross equity curve.

| Signal | Basis | Ann. return | Ann. vol | Sharpe | Max DD | Drawdown period | Calmar | Hit rate | Ann. turnover | Avg. holding (days) |
|---|---:|---:|---:|---:|---:|---|---:|---:|---:|---:|
| carry | gross | 4.88% | 4.35% | 1.12 | -14.24% | 2025-02-06 to 2026-08-28 | 0.34 | 52.4% | 2.70x | 237.0 |
| carry | net | 4.73% | 4.35% | 1.09 | -14.39% | 2025-02-06 to 2026-08-28 | 0.33 | 52.4% | 2.70x | 237.0 |
| momentum | gross | 12.35% | 4.00% | 2.93 | -9.80% | 2026-03-03 to 2026-07-07 | 1.26 | 56.6% | 8.44x | 80.3 |
| momentum | net | 11.88% | 4.01% | 2.82 | -9.97% | 2026-03-03 to 2026-07-07 | 1.19 | 56.3% | 8.44x | 80.3 |
| macro_surprise | gross | 0.41% | 4.02% | 0.12 | -9.94% | 2020-08-31 to 2022-01-19 | 0.04 | 50.4% | 6.99x | 92.2 |
| macro_surprise | net | 0.06% | 4.02% | 0.03 | -10.47% | 2020-08-31 to 2022-01-19 | 0.01 | 50.2% | 6.99x | 92.2 |
| composite | gross | 9.78% | 4.07% | 2.32 | -10.96% | 2026-01-07 to 2026-08-11 | 0.89 | 56.0% | 5.99x | 105.0 |
| composite | net | 9.45% | 4.07% | 2.24 | -11.20% | 2026-01-07 to 2026-08-11 | 0.84 | 55.6% | 5.99x | 105.0 |

The composite weights are 0.40 carry, 0.35 momentum and 0.25 macro surprise.
Carry is slow (2.70x annual turnover, 237-day average holding); momentum trades
most (8.44x, 80 days). Costs matter most for macro, whose net Sharpe falls from
0.12 at 0 bp to -0.22 at 20 bp.

### Cost sensitivity — net Sharpe

| Signal | 0 bp | 1 bp | 2 bp | 5 bp | 10 bp | 20 bp |
|---|---:|---:|---:|---:|---:|---:|
| carry | 1.12 | 1.11 | 1.10 | 1.09 | 1.05 | 0.99 |
| momentum | 2.93 | 2.91 | 2.89 | 2.82 | 2.71 | 2.48 |
| macro_surprise | 0.12 | 0.10 | 0.09 | 0.03 | -0.05 | -0.22 |
| composite | 2.32 | 2.30 | 2.29 | 2.24 | 2.17 | 2.01 |

### Validation gate

| Fixture set | Checks run | Checks fired | FAIL rows | Outcome |
|---|---:|---:|---:|---|
| Clean `inst/fixtures` | 6 | 0 | 0 | PASS — signals published |
| Broken `inst/fixtures/broken` | 6 | 6 | 6 | BLOCKED — exit 1 |

On the broken set, five failures belong to the corrupted `US_PMI` series —
`stale_series`, `gap_in_history`, `constant_series`, `zscore_outlier` and
`future_vintage` — and one belongs to a phantom `US_UNRATE` metadata row with no
observations (`missing_series`). The run is readable in
`results/validation_broken_summary.csv`; the command
`Rscript scripts/validate.R --fixtures inst/fixtures/broken` prints the blocking
series and exits non-zero.

### Tests and static checks

`make test` runs **25 test blocks containing 88 assertions**, all passing, with no
skips. `make lint` reports `lintr: clean` across `R/`. The suite includes explicit
no-lookahead assertions (a revised CPI vintage is invisible before its publication
date and visible after), a test that corrupting post-decision vintages leaves the
macro signal unchanged, dollar-neutral/gross-one weight tests, hand-built drawdown
arithmetic, zero-turnover constant-signal behaviour, golden metrics, and R/VBA
ranking parity against `tests/vba/ranking_cases.csv`.

## Reproducibility and limitations

| Item | Value |
|---|---|
| Run id | `run_20261113_c9f439e3` |
| As-of date | 2026-11-13 |
| Params file | `config/params.yml` |
| Params MD5 | `c9f439e3fc44c9408b5d3062e28198d1` |
| Fixture seed | 20261114 |
| R version | R version 4.6.1 (2026-06-24) |
| Platform | aarch64-apple-darwin25.4.0 |

Every number in this README was produced by the committed code in this session.
The fixtures are synthetic and are **not** a market-calibrated dataset: they are a
controlled demonstration that the vintage store, publish gate, point-in-time
signal construction, backtest accounting and reporting chain work end to end.

Specific limitations:

- **Momentum is deliberately strong in the simulation.** The fixture generator
  embeds a persistent trend component, so the 2.82 net Sharpe is a property of the
  seeded world and a check that the pipeline can recover a planted effect — not a
  forecast of market momentum.
- **The stand-alone macro signal is close to flat.** The lagged macro response is
  small relative to the noise and monthly publication timing, so its net Sharpe is
  0.03. That is an honest null result from this fixture set, not a hidden failure;
  the composite still benefits from diversification.
- **Costs are a constant one-way bp charge** on traded notional. There is no
  market impact, borrow, financing, slippage, taxes or capacity model. Turnover is
  reported so the assumption can be stressed with the cost grid.
- **The market panel is synthetic.** FX/rates/equity prices, carry and macro
  releases are simulated. The optional FRED path fetches macro series only and
  keeps the fixture market panel; it is not the source of the committed numbers.
- **Excel is a generated `.xlsx`, not a signed add-in.** The VBA modules must be
  imported and the workbook saved as `.xlsm`; macros require an Excel desktop
  installation and `Rscript` on PATH. openxlsx 4.2.9 has no chart API, so
  `R/excel_export.R` injects a standard OOXML chart part; the result is a native
  Excel line chart, verified with openpyxl (one `LineChart`, four series) and
  openxlsx read-back (six sheets, `rngRanked`, `rngMetrics`, `rngStatus`).

This is research infrastructure, not investment advice.
