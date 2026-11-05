# =============================================================================
#  01_import_validate.R  —  read the linkage input and the wide lab register,
#  validate the schema, fail early with a clear message. Reads participant-level
#  data: run only when data processing is authorised (files in data/, or paths in
#  config.local.R).
# =============================================================================
stopifnot(exists("LINK_INPUT"), exists("WIDE_LAB"))
if (!file.exists(LINK_INPUT)) stop("LINK_INPUT not found: ", LINK_INPUT,
  "\n  -> put the linkage file in data/ (see data/README.md) or set LINK_INPUT in config.local.R.")
# WIDE_LAB is optional: "" means none in data/ -> result comes from the linkage file itself.
has_wide <- nzchar(WIDE_LAB)
if (has_wide && !file.exists(WIDE_LAB)) stop("WIDE_LAB set but not found: ", WIDE_LAB,
  "\n  -> put the wide lab register in data/ or fix the path in config.local.R (or remove it to run without it).")

LINK <- rc(LINK_INPUT)
WIDE <- if (has_wide) rc(WIDE_LAB) else
  data.frame(patient_uid = character(0), stringsAsFactors = FALSE, check.names = FALSE)

# minimum columns the cohort stage needs (warn on soft-missing, stop on hard-missing)
need_link <- c("name_1","name_4","name_5","lab_sample_id","sex","age_years",
               "vaccin_mve","date_vaccination1","date_frist_sympt","match_type")
miss_l <- setdiff(need_link, names(LINK))
if (length(miss_l)) warning("[01] LINK_INPUT missing columns (derivations will blank them): ",
                            paste(miss_l, collapse = ", "))
if (has_wide) {
  if (!"patient_uid" %in% names(WIDE)) stop("[01] WIDE_LAB must have a patient_uid column (join key).")
  if (!any(grepl("^final_result_evd_[0-9]+$", names(WIDE))))
    warning("[01] WIDE_LAB has no final_result_evd_* columns — RT-PCR history cannot be built from it.")
  cat(sprintf("[01] linkage rows = %d (%d cols) | wide lab rows = %d (%d cols)\n",
              nrow(LINK), ncol(LINK), nrow(WIDE), ncol(WIDE)))
} else {
  # must be able to read a single result from the linkage file instead
  res_cands <- c("episode_result","FINAL RESULT (EVD)2","evd_result_final",
                 "final_result","pcr_result","pcr_test_result","FINAL RESULT (EVD)")
  if (!any(res_cands %in% names(LINK)))
    warning("[01] No wide lab AND no recognisable result column in the linkage file ",
            "(looked for: ", paste(res_cands, collapse = ", "), ") — outcome will be blank.")
  cat(sprintf("[01] linkage rows = %d (%d cols) | wide lab = NONE (result taken from linkage file)\n",
              nrow(LINK), ncol(LINK)))
}
