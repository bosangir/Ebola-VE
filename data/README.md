# data/ — local linked-data drop folder (NEVER committed)

Put the linked participant files here and the pipeline uses them automatically — no
config edit needed. Everything in this folder except this README is gitignored.

## Files the pipeline looks for (first match wins)
| Role | Drop a file named (any of) |
|---|---|
| Linkage input (one row per surveillance↔lab pair) | priority order: `cleaned_linkage.csv` → `linked_dataset_MAIN_distinct_cases.csv` → `linked_confirmed.csv` → `linked_dataset_MAIN.csv` → `linked_database.csv` → `linkage.csv` → `linked.csv` |
| Wide laboratory register (uid-keyed RT-PCR history) | `merged_eve_lab_wide.csv`, `wide_lab.csv` |
| (optional) dashboard resolution JSONs | `cleaning_status.json`, `deletions.json`, `record_corrections.json`, `result_overrides.json` |

If none are present, the pipeline stops with a clear message (no data is bundled).
To point elsewhere instead, set the paths in `config.local.R` (that overrides this folder).

## Expected columns (names only — see data_dictionary/)
- **Linkage input**: `name_1, name_4, name_5, lab_sample_id, sex, age_years, vaccin_mve,
  date_vaccination1, type_vaccin1, vaccine_strategy, date_frist_sympt, specimen_collection_date,
  outcome, ever_hospitalised, contact_connu, contact, funerailles, voyage, health_zone_1,
  province_1, health_area_1, village_residence, occupation, type_personnel, infection, carte,
  source_vacccination, sex_agreement, age_diff, match_type, final_status, linkage_tier, labreg_name`
- **Wide lab**: `patient_uid, final_result_evd_1..N, test_result_date_1..N, sex, age_years,
  health_zone, health_area, final_status, pcr_test_result, date_of_death`

Column names here are documented in `data_dictionary/Distinct_CODED_data_dictionary.csv`.
The analysis never needs the data values to be read by anyone but you.
