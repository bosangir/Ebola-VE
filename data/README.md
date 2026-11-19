# Data

| File | Contents |
| :--- | :--- |
| `EVE_VE_analysis_dataset_deidentified.csv` | Analysis dataset: one row per participant (392 rows, 92 variables) |
| `EVE_VE_analysis_dataset_deidentified.xlsx` | The same data, with the data dictionary on a second sheet |

## How the dataset was built

1. **Start:** the confirmed surveillance–laboratory linkage export (414 records), joined to the triangulated laboratory register for the full RT-PCR history of each person.
2. **Site QC feedback:** applied from the data-cleaning dashboard (254 corrected values, merge decisions and deletions checked).
3. **De-duplication:** one row per person, using a canonical identity key (22 duplicates consolidated).
4. **Derived variables:** outcome, exposure, timing and other analysis variables.
5. **De-identification:** names, case and laboratory IDs, village and health-worker role were removed. `study_id` (EVE-0001 to EVE-0392) is the only key.

Steps 1 to 5 are `R/01_import_validate.R` and `R/02_build_cohort.R`. They run only on the study team's computer, where the raw inputs are kept. Every variable is described in [`../data_dictionary/DATA_DICTIONARY.md`](../data_dictionary/DATA_DICTIONARY.md).

## What is not here

The raw linkage export, laboratory register, site QC files and the study-number crosswalk all identify people. They are kept in a private folder outside this repository and must not be added to it.

## Handling

This file has no names or IDs. It still holds exact dates (onset, specimen, vaccination, death), exact age, sex and health area for patients in a small outbreak. Together these could identify someone, so:

- keep the repository **private**;
- share the data only under the study's data-sharing terms;
- coarsen the dates (for example, to week) before any public release.
