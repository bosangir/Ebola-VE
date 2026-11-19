# =============================================================================
#  run_all.R  -  single entry point for the EVE vaccine-effectiveness analysis.
#
#  From the repository root:   Rscript Rscripts/run_all.R
#  Or open EVE_VE_Analysis.Rproj in RStudio and source this file.
#
#  FULL mode (study team, private raw inputs available):
#     01 import -> 02 site QC, de-duplication, de-identification -> 03 -> 04
#  ANALYSIS-ONLY mode (anyone else): 03 -> 04 from data/
#  The report and slides are written separately (Report/), not by R.
# =============================================================================
.here <- tryCatch(dirname(normalizePath(sub("^--file=", "",
           grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), error = function(e) NA)
PROJ <- if (!is.na(.here) && basename(.here) == "Rscripts") dirname(.here) else
        if (file.exists(file.path(getwd(), "Rscripts", "00_config.R"))) getwd() else dirname(getwd())

source(file.path(PROJ, "Rscripts", "00_config.R"), local = FALSE)
STEPS <- c(if (PRIVATE_MODE) c("01_import_validate.R", "02_build_cohort.R"),
           "03_main_analysis.R", "04_extended_analysis.R")
for (s in STEPS) { cat("\n========== ", s, " ==========\n"); source(file.path(SCRIPTS, s), local = FALSE) }
cat(sprintf("\n[done] de-identified dataset: %s\n[done] tables and figures: %s\n", DEID_OUT, OUT_DIR))
