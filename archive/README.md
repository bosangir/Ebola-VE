# archive/ — superseded scripts (kept as reference, not run)

These were moved here during the Nov-2026 reorganisation. The active workflow is
`run_all.R` + `R/00..R/04`. Nothing in the pipeline sources anything here.

## Reproduced by the new R/ pipeline (delete once R/ is validated on real data)
- `build_distinct_coded.R`      -> R/02_build_cohort.R (+ R/data_dictionary.R)
- `analyze_coded_report.R`      -> R/03_analysis.R + R/04_report.R
- `analyze_protocol_window.R`   -> R/03_analysis.R (protocol >10d + 0-10d/reclass contrasts, exposure-risk-adjusted models, clinical/contact/funeral characteristics, severity-among-cases) [FULLY PORTED]
                                    
- `build_meeting_report_001.R`  -> optional meeting deck; repoint inputs to outputs/analysis_tables.xlsx

## Superseded earlier analysis chains (historical)
- `eve_main_analysis.R` -> `ve_report_main.R`   (legacy MAIN-linkage chain)
- `ve_analysis.R`       -> `ve_report.R`         (oldest chain; T1..T11 tables)
- `build_distinct_case_coded.R`                  (old coded builder; wrote into the dashboard folder)

## Linkage / QC tooling (belongs to the dashboard pipeline, not the analysis)
- `reconcile_linkage.R`, `orphan_reconcile.R`, `build_orphan_dashboard.R`, `build_site_query_workbook.R`

Historical reports (`EVE_VE_Preliminary_Report*.docx`, the PowerPoint) are preserved
in the project root / Downloads as references.
