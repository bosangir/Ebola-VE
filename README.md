# Ebola Vaccine Effectiveness in the DRC (Bulape outbreak)

This repository contains a test-negative evaluation of the effectiveness of Ervebo (rVSV-ZEBOV) against laboratory-confirmed Ebola disease. The data come from the Bulape outbreak in the Democratic Republic of the Congo, 2025. It holds the R code, the linked and de-identified analysis dataset, the result tables and figures, and the final report.

**Status:** final analysis, 18 November 2026, for internal review. Do not cite or communicate externally until the study team agrees the conclusions.

## Main result

Vaccination was recorded for 3 of 27 cases (11%) and 96 of 290 test-negative controls (33%). That gives a crude vaccine effectiveness of 74.7% (95% CI 25.4 to 94.1).

Most of this crude effect comes from timing: most cases fell ill before vaccination began on 13 September 2025, and most controls were tested afterwards. Accounting for calendar time changes the estimate:

- adjusted for age, sex and calendar time: 52.5% (−43.6 to 88.0);
- matched on fortnight of onset: −0.7% (−330 to 76).

Under the protocol definition (vaccinated more than 10 days before onset), no case was vaccinated, compared with 23 of 217 controls.

The evaluation is therefore inconclusive for this outbreak. This means the data cannot settle the question; it is not evidence that the vaccine does not work.

## Repository structure

```
EVE_VE_Analysis_GitHub/
├── README.md                     this file
├── DATA_DICTIONARY.md            every variable, readable on GitHub
├── DATA_DICTIONARY.csv           the same, machine-readable (written by the pipeline)
├── EVE_VE_Analysis.Rproj         RStudio project
├── config.local.R.example        optional template for private paths
├── Rscripts/
│   ├── run_all.R                 single entry point
│   ├── 00_config.R               packages, folders, helpers, run mode
│   ├── 01_import_validate.R      reads the raw linkage export and lab register   (full mode)
│   ├── 02_build_cohort.R         site QC, de-duplication, de-identification      (full mode)
│   ├── 02a_apply_site_qc.R       applies the dashboard QC decisions              (full mode)
│   ├── 02b_data_dictionary.R     builds the data dictionary                      (full mode)
│   ├── 03_main_analysis.R        main VE models, sensitivity, mortality, F1-F5
│   └── 04_extended_analysis.R    tables, IPTW, latent classes, subgroups, death,
│                                 co-infection, matched analysis, F6-F14
├── Data/
│   ├── EVE_VE_analysis_dataset_deidentified.csv   392 participants, de-identified
│   ├── EVE_VE_analysis_dataset_deidentified.xlsx
│   └── README.md                                  how the dataset was built
├── Outputs/                      aggregate tables and figures (regenerated on each run)
│   ├── cohort_flow.csv, qc_summary.csv, qc_breakdown.csv
│   ├── analysis_tables.xlsx, extended_tables.xlsx
│   └── figures/
└── Report/
    ├── EVE_VE_Preliminary_Report_v2.0_18Nov2026.docx / .pdf
    ├── EVE_VE_Final_Results_Slides_19Nov2026.pptx
    └── build_scripts/            Python scripts that lay out the report and slides
```

## How to run

You need R 4.6 or later, with these packages:

```r
install.packages(c("dplyr", "ggplot2", "writexl", "logistf", "sandwich", "tidyr", "survival", "jsonlite"))
```

From the repository root:

```bash
Rscript Rscripts/run_all.R
```

In RStudio, open `EVE_VE_Analysis.Rproj` and source `Rscripts/run_all.R`.

The pipeline chooses its mode by itself:

| Mode | When | What runs |
| :--- | :--- | :--- |
| Analysis only | Anyone with this repository | `03` and `04`, from `Data/`. Rewrites `Outputs/`. |
| Full | The study team, with the private raw inputs present | `01` to `04`. Rebuilds `Data/` and `DATA_DICTIONARY.csv` from the 414-record linkage export, then analyses. |

Full mode looks for a private folder named `EVE_VE_PRIVATE_identifiers` next to the repository:

```
EVE_VE_PRIVATE_identifiers/
└── raw_inputs/
    ├── EVE_raw_linkage_export_414_records.csv
    ├── EVE_raw_lab_register_wide.csv
    └── site_qc_feedback/          record_corrections.json, merge_decisions.json, ...
```

If the folder is somewhere else, set the environment variable `EVE_PRIVATE_DIR`, or copy `config.local.R.example` to `config.local.R`. To force analysis-only mode, set `EVE_MODE=public`. Both modes give identical results.

## Methods in brief

**Linkage.** Surveillance case-investigation forms (575) were linked to a laboratory register built from six laboratory datasets (1,135 patients). Matching was by name in four tiers, followed by review: an exact full name with agreeing sex and age; two shared name tokens; name with sex and age within 5 years; partial name with sex. The confirmed export had 414 records. After applying the site QC feedback and removing duplicates, 392 distinct participants remained.

**Design.** Test-negative case-control. The outcome is the episode-defining RT-PCR result, meaning the first definitive specimen on or after symptom onset. The main exposure is any recorded vaccination. The protocol exposure is vaccination more than 10 days before onset. Unknown vaccination status is excluded and never imputed. VE = (1 − OR) × 100.

**Models.**

- Firth penalised logistic regression, with exact conditional estimates as a check.
- Adjustment for age, sex and calendar time, then also for contact and funeral exposure or latent exposure class.
- Inverse probability of treatment weighting.
- Conditional logistic regression matched on fortnight of onset.
- Restriction to the vaccination period.
- Subgroup interaction tests (protocol threshold p < 0.01).
- VE against death among cases, and against hospitalised and fatal disease.
- A description of co-infection.

**Report and slides.** These are written by hand from the result tables. R does not generate them. The scripts in `Report/build_scripts/` lay them out and read every number from `Outputs/`. To rebuild the report you also need the Word template it was built on.

## Data protection

- No names, case IDs, laboratory IDs or villages are in this repository.
- Identifiable files are written only to the private folder. `.gitignore` blocks the usual identifiable file names as a safety net.
- The de-identified dataset still holds exact dates, age, sex and health area. **Keep this repository private.** Coarsen dates before any public release.

## Known open items

- 120 site corrections could not be matched to a record, and 7 merge decisions concern records outside the linkage export. Both are listed for the site in the private QC log.
- The dashboard linkage code reads `dd-Mon-yy` dates as year 25 AD. The fix is in `Rscripts/00_config.R` (`pdate`) but has not yet been made in the dashboard.
- Onset-to-death intervals exist for 27 cases, but 18 deaths are recorded among cases. The death dates need reconciling.

## Licence

To be agreed by the study team before any public release.
