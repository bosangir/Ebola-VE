# EVE / VE — Preliminary Vaccine-Effectiveness Analysis (DRC, test-negative design)

Reproducible R workflow for the preliminary analysis presented on 8 Oct 2026
(Report 001). It produces a **clean coded analysis dataset** that follows the study
flowchart, the **analysis outputs** (tables, figures), and a **preliminary report**
generated from those outputs.

> **PRELIMINARY — internal review.** All estimates are provisional.

## Quick start
```bash
# from the project root
"C:/Program Files/R/R-4.6.0/bin/Rscript.exe" run_all.R
```
**To run, drop the linked files in the local `data/` folder** (see `data/README.md`) and run
the command above — the pipeline finds them automatically, and `data/` is gitignored. No
config edit needed. `config.local.R` is only for pointing elsewhere. With no inputs in `data/`
and no `config.local.R`, the pipeline stops with a clear message (no data is bundled).

## Run order (one entry point: `run_all.R`)
| Step | Script | Does |
|---|---|---|
| 0 | `R/00_config.R` | packages, paths (from `config.local.R`), helpers, coding maps, VE transform |
| 1 | `R/01_import_validate.R` | read linkage + wide lab register; validate schema |
| 2 | `R/02_build_cohort.R` | build coded dataset (one row/participant) + **study-flow table** (+ `R/data_dictionary.R`) |
| 3 | `R/03_analysis.R` | EDA, descriptives, **crude + adjusted VE**, sensitivity; saves a results bundle |
| 4 | `R/04_report.R` | **Word report** from the saved bundle (values stay out of the report source) |

Run a single stage interactively by sourcing `R/00_config.R` then the stage you want.

## Configure your data (private; never committed)
```r
# config.local.R  (copy from config.local.R.example)
LINK_INPUT <- "…/cleaned_linkage.csv"        # one row per surveillance<->lab pair
WIDE_LAB   <- "…/merged_eve_lab_wide.csv"    # uid-keyed RT-PCR history (patient_uid)
DASH_DATA  <- ""                             # optional: dashboard JSONs (deletions/corrections/result_overrides)
OUT_DIR    <- "…/private/EVE_VE_outputs"     # private outputs (identifiable)
```

## Expected input schema (key columns)
- **LINK_INPUT**: `name_1,name_4,name_5,lab_sample_id,sex,age_years,vaccin_mve,date_vaccination1,
  type_vaccin1,vaccine_strategy,date_frist_sympt,specimen_collection_date,outcome,ever_hospitalised,
  contact_connu,contact,funerailles,voyage,health_zone_1,…,match_type,final_status,linkage_tier,labreg_name`
- **WIDE_LAB**: `patient_uid, final_result_evd_1..N, test_result_date_1..N, sex, age_years, health_zone,
  health_area, final_status, pcr_test_result, date_of_death`

See `data_dictionary/Distinct_CODED_data_dictionary.csv` for the full coded variable list.

## Methods (scope = Report 001 slides)
Test-negative design. **Outcome** = episode-defining RT-PCR (first definitive specimen on/after
onset); case = positive, control = negative; indeterminate excluded. **Exposure** = vaccinated vs
not vaccinated (unknown excluded, never imputed); protocol >10-day effective window as a sensitivity.
**Model**: logistic, crude then adjusted (age group, sex, calendar week); `VE = (1 − OR) × 100`,
profile-likelihood 95% CI. **Sensitivity**: Firth penalised, exclude cross-record links, alternative
outcome definitions.

## Outputs (`OUT_DIR`, default `outputs/`)
`Distinct_CODED.csv/.xlsx`, `Distinct_CODED_data_dictionary.csv`, `cohort_flow.csv`,
`analytic_participant_level.csv`, `analysis_tables.xlsx`, `results_bundle.rds`, `figures/*.png`,
`EVE_VE_Preliminary_Report_<date>.docx`.

## R dependencies
`dplyr`, `ggplot2`, `writexl` (stages 0–3); `officer`, `flextable` (stage 4);
`jsonlite` (optional, only if `DASH_DATA` is set); `readxl` (optional). R 4.6.x.
```r
install.packages(c("dplyr","ggplot2","writexl","officer","flextable","jsonlite"))
```

## Checks that still require a run on the REAL data
- Cohort counts reconcile with the slides (export → distinct → eligible → primary → adjusted).
  Updated/cleaned data may differ from the Oct-2026 slide counts.
- Firth **adjusted** model fits (can be skipped when data are sparse/separable).
- Vaccination-date / strategy completeness and the differential-missingness pattern.
- The data-quality flags (onset-to-swab > 30 days, vaccinated-after-onset, indeterminate results).

## Data handling & privacy
Participant data is never committed. The `data/` drop-folder, `config.local.R`, all generated
outputs, participant-level files, QC logs, and reports are gitignored. Only code, the README,
and the shareable data dictionary are tracked.

## Repository layout
```
run_all.R  config.local.R.example  README.md  .gitignore
R/            00..04 pipeline + data_dictionary.R
data_dictionary/  shareable coded variable dictionary
data/             local linked-data drop folder (gitignored; see data/README.md)
archive/          superseded scripts (see archive/README.md)
outputs/          generated (gitignored)
```
