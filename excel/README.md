# MacroSignals Desk — Excel/VBA front end

The workbook `MacroSignals_Desk.xlsx` is generated from the R results by:

```sh
make excel
```

It contains six sheets:

| Sheet | Contents |
|---|---|
| `Dashboard` | Run id, as-of date, params MD5, data-quality status light and headline net metrics |
| `Ranked_Signals` | Ranked composite cross-section with a colour scale on the z-score columns; Excel table `tblRanked` |
| `Metrics` | Gross and net diagnostics for carry, momentum, macro surprise and the composite |
| `EquityCurve` | Daily cumulative net curves plus a native Excel line chart |
| `Validation` | The validation log for the run (`FAIL` rows highlighted red) |
| `Params` | Every version-controlled parameter from `config/params.yml` |

Named ranges: `rngRanked`, `rngMetrics`, `rngStatus`.

The `.xlsx` is generated with **openxlsx**. openxlsx 4.2.9 does not expose a
chart API, so `R/excel_export.R` saves the workbook with openxlsx and then adds
a first-class OOXML chart part (`xl/charts/chart1.xml`) to the package. The
result is a native Excel line chart, not an embedded image.

## One-time setup: import the VBA modules and save as `.xlsm`

1. Open `excel/MacroSignals_Desk.xlsx` in Excel (Windows or Mac).
2. Press **Alt+F11** (Windows) or **Option+F11** (Mac) to open the Visual Basic Editor.
3. In the VBA editor choose **File → Import File…** (or **Insert → Module** and
   paste the module text). Import all five files from `vba/`:
   - `modRefresh.bas`
   - `modLoad.bas`
   - `modRank.bas`
   - `modValidation.bas`
   - `modTests.bas`
4. Close the editor. Choose **File → Save As**, set *Save as type* to
   **Excel Macro-Enabled Workbook (*.xlsm)**, and save as
   `excel/MacroSignals_Desk.xlsm`.
5. If Excel prompts about macros, enable them for this file. On macOS you may
   need to grant Excel access to the folder containing the project.

The `.xlsm` is intentionally not committed: it is a local, macro-enabled copy of
the version-controlled `.xlsx` and `.bas` sources. The generated `.xlsx` is
committed so the workbook can be inspected without enabling macros.

## The PM refresh workflow

1. Open `MacroSignals_Desk.xlsm`.
2. Check the **Dashboard** status light:
   - green `PASS` — the last validation run had no FAIL rows;
   - red `FAIL (n)` — publication is blocked.
3. Press **Alt+F8**, select `RefreshSignals`, and click **Run**.
   `RefreshSignals` will:
   - call `CheckStatus()` and refuse to run if the Validation sheet has FAIL rows;
   - shell out to `Rscript scripts/run_research.R` from the project root and wait;
   - reload `results/ranked_latest.csv`, `results/metrics.csv`,
     `results/equity_curve.csv` and `results/validation_log.csv`;
   - timestamp the Dashboard and refresh the data-quality light.

If `Rscript` is not on the PATH, add the R `bin` directory to PATH (or launch
Excel from a terminal where `Rscript` resolves) before clicking refresh.

## Rankings and filters

In any spare cell:

- `=RankSignals(E2:E17)` returns average ranks (1 = largest z-score) as a
  column array. Enter with **Ctrl+Shift+Enter** in older Excel versions.
- `=TopN(E2:E17, 5)` returns the five largest values in descending order.

The same logic is implemented in `R/rank_reference.R` and verified against
`tests/vba/ranking_cases.csv`. To run the in-Excel checks, press **Alt+F8**,
select `RunVbaTests`, and click **Run**. It writes the golden cases to a scratch
sheet, calls `RankSignals` and `TopN`, and reports the assertion count.

## Rebuilding after a FAIL

`RefreshSignals` deliberately blocks while the Validation sheet contains FAIL
rows. After fixing the data (or re-ingesting clean fixtures), rebuild the
workbook with `make excel`, reopen it, and import/refresh from there. This keeps
a corrupt run from silently overwriting a good desk view.
