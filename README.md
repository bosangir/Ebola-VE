<div align="center">

# Ebola Vaccine Effectiveness, Bulape

Reproducible R pipeline for a test-negative study of Ervebo (rVSV-ZEBOV) effectiveness against laboratory-confirmed Ebola virus disease, Democratic Republic of the Congo.

[![R](https://img.shields.io/badge/R-4.6.x-276DC3)](https://www.r-project.org/)
![Status](https://img.shields.io/badge/status-preliminary%20v2.0-orange)
![Data](https://img.shields.io/badge/participant%20data-not%20included-lightgrey)

</div>

<hr>

## Overview

This repository holds the code for the evaluation. It turns the linked outbreak data into a clean, de-duplicated and de-identified analysis dataset, then estimates vaccine effectiveness by several methods and produces the result tables and figures. It contains no participant data.

The design is a test-negative case-control study. Among suspected patients tested by RT-PCR, cases test positive and controls test negative. The two groups are then compared on prior vaccination. Vaccine effectiveness is VE = (1 - odds ratio) x 100.

The report and slides are written by the study team from the result tables. The R code does not write them.

## Quick start

1. Install R 4.6.x and the packages listed under Requirements.
2. Put the input files in place (see Data handling and privacy).
3. Run the whole pipeline from the repository root:

```bash
Rscript R/run_all.R
```

The pipeline picks its mode by itself:

| Mode | When | What runs |
| :--- | :--- | :--- |
| Full | The study team's computer, where the private raw inputs exist | Steps 1 to 4. Rebuilds the de-identified dataset from the 414-record linkage export, then analyses it. |
| Analysis only | Anyone with the de-identified dataset in `data/` | Steps 3 and 4 |

Set the environment variable `EVE_MODE=public` to force analysis-only mode. Both modes give the same results.

## Pipeline

| Step | Script | Produces |
| :--- | :--- | :--- |
| 0 | `R/00_config.R` | packages, folders, shared helpers, coding maps, run mode |
| 1 | `R/01_import_validate.R` | reads the linkage export and the laboratory register, checks the schema |
| 2 | `R/02_build_cohort.R` | one row per participant, derived variables, study flow, de-identified dataset |
| 2a | `R/02a_apply_site_qc.R` | applies the site QC feedback from the dashboard: deletions, corrected values, duplicate merges |
| 2b | `R/02b_data_dictionary.R` | builds the data dictionary |
| 3 | `R/03_main_analysis.R` | main VE models, sensitivity analyses, protocol window, mortality, figures F1 to F5 |
| 4 | `R/04_extended_analysis.R` | descriptive tables, propensity-score weighting, latent class model, subgroups, death and severe disease, co-infection, fortnight-matched analysis, figures F6 to F14 |

`R/run_all.R` runs the steps in order. Steps 1 and 2 run only in full mode.

## Data handling and privacy

Participant data never goes into this repository.

**Raw inputs.** The raw files identify people. They stay in a private folder outside the repository, by default a folder named `EVE_VE_PRIVATE_identifiers` next to it:

```
EVE_VE_PRIVATE_identifiers/
└── raw_inputs/
    ├── EVE_raw_linkage_export_414_records.csv
    ├── EVE_raw_lab_register_wide.csv
    └── site_qc_feedback/          record_corrections.json, merge_decisions.json, ...
```

These files are:

- the confirmed linkage export (414 records);
- the triangulated laboratory register;
- the site QC decisions.

To use another location, set `EVE_PRIVATE_DIR`, or copy `config.local.R.example` to `config.local.R` and set the paths there. `config.local.R` is gitignored.

**Outputs of a full run.** Every file that names a person goes to the private folder: the identifiable coded dataset, the study-number crosswalk and the QC log by laboratory ID. The shareable de-identified dataset is written to `data/`. Aggregate tables and figures go to `Outputs/`.

**De-identified dataset.** This file holds no names or IDs. It still contains exact dates, age, sex and health area, so it is shared only by the study team, under the study's data-sharing terms. It is not stored here.

## Requirements

R 4.6.x and the following packages:

```r
install.packages(c("dplyr", "ggplot2", "writexl", "logistf", "sandwich", "tidyr", "survival", "jsonlite"))
```

`jsonlite` is needed only in full mode, to read the site QC feedback.

## Outputs

**`data/`**
- `EVE_VE_analysis_dataset_deidentified.csv` and `.xlsx`: one row per participant, study number only, with the data dictionary on a second sheet.

**`Outputs/`**
- `cohort_flow.csv`: study-flow counts.
- `qc_summary.csv` and `qc_breakdown.csv`: what QC feedback was applied or not, as counts.
- `analysis_tables.xlsx` and `extended_tables.xlsx`: every result table, one per sheet.
- `figures/`: F1 to F14.

**Data dictionary**
- `data_dictionary/DATA_DICTIONARY.md` and `.csv`: every variable, including which ones are kept out of the shared dataset.

## Site QC feedback

Corrections, merges and deletions recorded in the dashboard are applied automatically in full mode. Corrections to surveillance fields are carried to every record of the same person, so they do not create false duplicates. Items that cannot be matched to exactly one surveillance-laboratory pair are listed in the private QC log for manual resolution.

In the current run:

- 254 of 374 corrected values were applied;
- 13 merge decisions and 9 deletions were checked;
- 22 duplicates were consolidated, leaving 392 distinct participants.

Dates written as `08-Sep-25` are now read as 2025. Before the fix they were read as the year 25 AD.

## Methods in brief

**Outcome.** The episode-defining RT-PCR result, meaning the first definitive specimen on or after symptom onset.

**Exposure.** The main exposure is any recorded vaccination. The protocol exposure counts a person as effectively vaccinated only if onset was more than 10 days after vaccination. Unknown status is excluded and never imputed.

**Models.**

- Firth penalised logistic regression, with exact conditional estimates as a check.
- Adjustment for age, sex and calendar time, then also for contact and funeral exposure or latent exposure class.
- Inverse probability of treatment weighting.
- Conditional logistic regression matched on fortnight of onset.
- Restriction to the vaccination period.
- Subgroup interaction tests.
- VE against death among cases, and against hospitalised and fatal disease.

**Main finding.** The crude VE was 74.7% (95% CI 25.4 to 94.1). It fell to 52.5% (-43.6 to 88.0) after adjustment for calendar time, and to -0.7% when cases and controls were matched on fortnight of onset. Most cases fell ill before vaccination began on 13 September 2025, so the evaluation is inconclusive for this outbreak.

## Status

Preliminary report, version 2.0 (18 November 2026), for internal review. All estimates are provisional.
