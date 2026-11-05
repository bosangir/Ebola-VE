<div align="center">

# Ebola Vaccine Effectiveness, Bulape

Reproducible R pipeline for a test-negative study of Ervebo (rVSV-ZEBOV) effectiveness against laboratory-confirmed Ebola virus disease, Democratic Republic of the Congo.

[![R](https://img.shields.io/badge/R-4.6.x-276DC3)](https://www.r-project.org/)
![Status](https://img.shields.io/badge/status-preliminary-orange)
![Data](https://img.shields.io/badge/participant%20data-not%20included-lightgrey)

</div>

<hr>

## Overview

This repository holds the code that turns linked outbreak data into a clean analysis dataset, the vaccine-effectiveness estimates, and the figures and reports. It contains no participant data. You supply your own linked files locally, and the pipeline builds everything from them.

The design is a test-negative case-control study. Among suspected patients tested by RT-PCR, cases test positive and controls test negative, and the two groups are compared on prior vaccination. Vaccine effectiveness is VE = (1 - odds ratio) x 100.

## Quick start

1. Install R 4.6.x and the packages listed under Requirements.
2. Put your linked files in the local `data/` folder (see `data/README.md`).
3. Run the whole pipeline:

```bash
Rscript run_all.R
```

On Windows you can double-click `run_all.bat` instead. Outputs are written to `outputs/`, which is never committed.

## Pipeline

| Step | Script | Produces |
| :--- | :--- | :--- |
| 0 | `R/00_config.R` | packages, paths, shared helpers, coding maps |
| 1 | `R/01_import_validate.R` | reads the linkage and laboratory files, checks the schema |
| 2 | `R/02_build_cohort.R` | one row per participant, coded dataset, data dictionary, study flow |
| 3 | `R/03_analysis.R` | VE models, sensitivity analyses, mortality, figures, results bundle |
| 4 | `R/04_report.R` | narrative Word report, built from the bundle |
| 5 | `R/05_slides.R` | PowerPoint deck, built from the bundle |

`run_all.R` runs the steps in order. Every result lives in the bundle, so the report and slides hold no hand-typed numbers.

## Data handling and privacy

Participant data stays on your machine. The `data/` drop folder and all generated outputs, reports, decks and logs are listed in `.gitignore` and are never pushed. Only the code, this README, the example configuration and the shareable data dictionary are tracked.

To point the pipeline at files outside `data/`, copy `config.local.R.example` to `config.local.R` and set the paths there. `config.local.R` is also gitignored.

## Requirements

R 4.6.x and the following packages:

```r
install.packages(c("dplyr", "ggplot2", "writexl", "officer", "flextable", "jsonlite"))
```

`officer` and `flextable` are needed for the report and slides. `jsonlite` is optional, used only when a dashboard cleaning folder is supplied.

## Outputs

Running the pipeline writes to `outputs/`:

- `Distinct_CODED.csv` and `.xlsx`, the coded one-row-per-participant dataset, with the data dictionary
- `EVE_Master_Distinct_<N>_cleaned.xlsx`, the de-duplicated master workbook
- `cohort_flow.csv`, the study-flow counts
- `analysis_tables.xlsx`, every analysis table on its own sheet
- `results_bundle.rds`, the saved results object
- `figures/F1` to `F5`, the plots
- the Word report and the PowerPoint deck

## Methods in brief

The outcome is the episode-defining RT-PCR result, meaning the first definitive specimen on or after symptom onset. The exposure is vaccinated against not vaccinated, with unknown status excluded and never imputed. Models are logistic regression, crude and adjusted, with Firth penalised regression where the data are sparse. Sensitivity analyses cover the exclusion of lower-confidence links, alternative outcome definitions, and the protocol window that counts a person as effectively vaccinated only more than 10 days before onset. A retrospective cohort estimates effectiveness against death among confirmed cases.

## Status

Preliminary and for internal review. All estimates are provisional.
