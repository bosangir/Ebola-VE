# Data dictionary

Variables in `Data/EVE_VE_analysis_dataset_deidentified.csv`: one row per participant (392 rows), linked, de-duplicated and de-identified. The same information is in `DATA_DICTIONARY.csv`, which the pipeline rewrites on every full run.

Codes follow the surveillance and laboratory forms: 1 = Yes, 2 = No, 99 = Unknown unless stated. A blank cell means the value was not recorded.

## Variables in the shared dataset (92)

| Variable | Description | Type | Source | Values / codes | Notes |
| :--- | :--- | :--- | :--- | :--- | :--- |
| `study_id` | Study identifier (sequential) | ID | Derived | EVE-0001... | Pseudonymous study ID (protocol sec 7) |
| `age_surv` | Age (years) - surveillance | numeric | Surveillance | >=0 |  |
| `age_lab` | Age (years) - laboratory | numeric | Laboratory | >=0 |  |
| `age` | Age (years) - resolved (surv preferred) | numeric | Derived | >=0 |  |
| `age_group` | Age group | categorical | Derived | 0-5;6-17;18-44;45+ |  |
| `age_group_code` | Age group code | integer | Derived | 1=0-5;2=6-17;3=18-44;4=45+ |  |
| `sex_surv` | Sex - surveillance | categorical | Surveillance | see sex_surv_code |  |
| `sex_surv_code` | Sex code - surveillance | code | Surveillance | 1=Male;2=Female;3=Unknown |  |
| `sex_lab` | Sex - laboratory | categorical | Laboratory |  |  |
| `sex_lab_code` | Sex code - laboratory | code | Laboratory | 1=Male;2=Female |  |
| `sex` | Sex - resolved | categorical | Derived |  |  |
| `sex_code` | Sex code - resolved | code | Derived | 1=Male;2=Female;3=Unknown |  |
| `sex_agreement` | Surv vs lab sex agree | boolean | Derived | True/False |  |
| `province` | Province | text | Surveillance |  | sec 6 geographic |
| `health_zone` | Health zone | text | Surveillance |  | sec 6 geographic |
| `health_area` | Health area | text | Surveillance |  | sec 6 geographic |
| `health_zone_lab` | Health zone - laboratory | text | Laboratory |  |  |
| `health_area_lab` | Health area - laboratory | text | Laboratory |  |  |
| `occupation_code` | Occupation (coded) | code | Surveillance | 1..13,99 | NO decode key in source |
| `hcw_frontline` | Healthcare/frontline worker | categorical | Derived | Yes/Unknown | Yes if a role recorded |
| `hcw_frontline_code` | HCW/frontline code | code | Derived | 1=Yes;99=Unknown |  |
| `prior_ebod` | Prior documented EBOD history | categorical | Surveillance | Yes/No/Unknown | from 'infection' |
| `prior_ebod_code` | Prior EBOD code | code | Surveillance | 1=Yes;2=No;99/0=Unknown |  |
| `vacc_status` | Vaccination status | categorical | Surveillance | Vaccinated/Not vaccinated/Unknown | register not available - surveillance field |
| `vacc_status_code` | Vaccination status code | code | Surveillance | 1=Vaccinated;2=Not;99=Unknown | PRIMARY exposure |
| `vacc_product` | Vaccine product | categorical | Surveillance |  | type_vaccin1 |
| `vacc_product_code` | Vaccine product code | code | Surveillance | 1;2;99 |  |
| `n_doses` | Number of doses | integer | Derived | 0,1,2; ''=unknown | count of vaccination dates |
| `vacc_date1` | Vaccination date - dose 1 | date | Surveillance | YYYY-MM-DD |  |
| `vacc_date2` | Vaccination date - dose 2 | date | Surveillance | YYYY-MM-DD |  |
| `vacc_source_code` | Source of vaccination info (coded) | code | Surveillance | 1,2,3,99 | NO decode key |
| `vacc_strategy_code` | Vaccination strategy (coded) | code | Surveillance | 1..5 | NO decode key - ring vs geographic NOT resolvable |
| `vacc_card` | Vaccination card seen | categorical | Surveillance | Yes/No/Unknown |  |
| `vacc_card_code` | Vaccination card code | code | Surveillance | 1=Yes;2=No;99/0=Unknown |  |
| `onset_date` | Date of symptom onset | date | Surveillance | YYYY-MM-DD | sec 6; calendar-time anchor |
| `specimen_date_first` | Date of (first) specimen | date | Laboratory/Surv | YYYY-MM-DD | first lab date else surveillance specimen |
| `n_tests` | Number of definitive RT-PCR tests | integer | Laboratory | >=1 |  |
| `first_test_date` | Date of first definitive test | date | Laboratory |  |  |
| `first_result` | First RT-PCR result | categorical | Laboratory | Positive/Negative |  |
| `first_result_code` | First result code | code | Laboratory | 1=Positive;0=Negative |  |
| `intermediate_results` | Intermediate results | text | Laboratory | semicolon-separated |  |
| `last_test_date` | Date of last definitive test | date | Laboratory |  |  |
| `last_result` | Last RT-PCR result | categorical | Laboratory | Positive/Negative |  |
| `last_result_code` | Last result code | code | Laboratory | 1=Positive;0=Negative |  |
| `episode_test_date` | Date of episode-defining specimen | date | Derived |  | first definitive on/after onset |
| `episode_result` | Episode-defining RT-PCR result | categorical | Derived | Positive/Negative | PROTOCOL PRIMARY outcome |
| `episode_result_code` | Episode-defining result code | code | Derived | 1=Positive(case);0=Negative(control) | PRIMARY analytic outcome Y |
| `any_positive` | Any positive RT-PCR (ever) | categorical | Laboratory | Yes/No |  |
| `date_first_positive` | Date of first positive | date | Laboratory |  |  |
| `date_last_positive` | Date of last positive | date | Laboratory |  |  |
| `result_trajectory` | Result sequence (P/N over time) | text | Laboratory | e.g. PPN |  |
| `pcr_result_patient` | Patient-level RT-PCR result | categorical | Laboratory | Positive/Negative/... |  |
| `case_confirmed` | Case / control (any-positive) | categorical | Derived | Confirmed case/Test-negative control/Indeterminate |  |
| `case_confirmed_code` | Case/control code (any-positive) | code | Derived | 1=Case;0=Control;''=indeterminate |  |
| `orthoebolavirus_species` | Identified species | categorical | Laboratory | (blank) | NOT RECORDED; Zaire ebolavirus assumed |
| `vital_status` | Vital status (alive/dead) | categorical | Surv+Lab | Alive/Dead/Unknown | sec 6 outcome/mortality |
| `vital_status_code` | Vital status code | code | Derived | 1=Alive;2=Dead;99=Unknown |  |
| `date_of_death` | Date of death | date | Laboratory |  |  |
| `ever_hospitalised` | Ever hospitalised (severity proxy) | categorical | Surveillance | Yes/No | sec 6 clinical severity |
| `n_hospitalisations` | Number of hospitalisations | integer | Surveillance | >=0 |  |
| `contact_known` | Contact known | categorical | Surveillance | Yes/No/Unknown | sec 6 exposure/contact |
| `contact_known_code` | Contact known code | code | Surveillance | 1=Yes;2=No;99=Unknown |  |
| `contact_with_case` | Contact with a case | categorical | Surveillance | Yes/No/Unknown |  |
| `contact_with_case_code` | Contact with case code | code | Surveillance | 1=Yes;2=No;99=Unknown |  |
| `funeral_participation` | Funeral participation | categorical | Surveillance | Yes/No/Unknown | exposure |
| `funeral_code` | Funeral code | code | Surveillance | 1=Yes;2=No;99=Unknown |  |
| `travel` | Travel history | categorical | Surveillance | Yes/No/Unknown | exposure |
| `travel_code` | Travel code | code | Surveillance | 1=Yes;2=No;99=Unknown |  |
| `burial_attended` | Attended a burial | categorical | Surveillance | Yes/No/Unknown | attended_burial; exposure |
| `n_burials_attended` | Number of burials attended | integer | Surveillance | >=0 |  |
| `n_contacts` | Number of contacts with a case reported | integer | Surveillance | >=0 |  |
| `contact_followed` | Was a listed contact under follow-up | categorical | Surveillance | Yes/No/Unknown/Other | contact_suivi; ring-vaccination eligibility proxy |
| `animal_contact` | Contact with animals | categorical | Surveillance | Yes/No/Unknown/Other | animal |
| `traditional_healer` | Visited a traditional healer | categorical | Surveillance | Yes/No/Unknown | tradipraticien |
| `hemorrhage` | Haemorrhagic signs | categorical | Surveillance | Yes/No/Unknown | hemorragie; severity |
| `pregnant` | Pregnant (women) | categorical | Surveillance | Yes/No/Unknown |  |
| `death_date` | Date of death | date | Surv+Lab | YYYY-MM-DD | date_sortie_deces if dead, else lab date_of_death |
| `admission_date` | Date of admission (laboratory register) | date | Laboratory | YYYY-MM-DD |  |
| `sampled_dead` | Specimen taken after death | categorical | Laboratory | Yes/No | any status_at_sampling = Dead/Deceased (post-mortem swab) |
| `biofire_tested` | Tested on BioFire panel | categorical | Laboratory | Yes/No | any BioFire result other than 'not tested' |
| `malaria_detected` | Malaria recorded (co-infection) | categorical | Laboratory | Yes/No | Plasmodium on BioFire or malaria noted in lab comments/treatment; 'No' = not recorded, not a negative test |
| `ct_gp_min` | Lowest GeneXpert Ct (GP target) | numeric | Laboratory | Ct | lower Ct = higher viral load |
| `ct_np_min` | Lowest GeneXpert Ct (NP target) | numeric | Laboratory | Ct |  |
| `linkage_tier` | Linkage tier | categorical | Linkage | Tier 1..4 / Cross-record |  |
| `linkage_final_status` | Linkage acceptance status | categorical | Linkage | Auto/Reviewer/Cross-accepted |  |
| `match_type` | Match type | categorical | Linkage | within-pair/cross-record |  |
| `is_cross_record` | Cross-record (orphan T3/T4) link | categorical | Linkage | Yes/No | Yes = lower-confidence; exclude in sensitivity |
| `age_diff` | Age difference (surv vs lab) | numeric | Derived | years |  |
| `result_overridden` | Site-verified result override | categorical | Site/Lab | Positive/Negative/Invalid/blank | non-blank = episode result set by a site 'Verify lab result' response |
| `qc_corrected_fields` | Fields corrected from site QC feedback | text | Site QC (dashboard) | semicolon-separated field names; blank = none | from record_corrections.json; details in the private QC log |
| `qc_merged` | Record kept after a QC duplicate merge | categorical | Site QC (dashboard) | Yes/No | Yes = duplicate record(s) merged into this one (merge_decisions.json) |
| `village_recorded` | Village of residence recorded | categorical | Derived | Yes/No | village name itself is kept only in the private file |

## Identifying variables kept out of the shared dataset (6)

These exist only in the private folder, linked to `study_id` through a crosswalk. They are listed so the structure is documented; their values are never shared.

| Variable | Description | Source |
| :--- | :--- | :--- |
| `record_id` | Surveillance case identifier | Surveillance |
| `lab_patient_uid` | Laboratory patient identifier | Laboratory |
| `firstname` | First name | Surveillance |
| `surname` | Surname / second name | Surveillance |
| `village` | Village | Surveillance |
| `hcw_frontline_role` | HCW/frontline role (text) | Surveillance |
