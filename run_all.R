# =============================================================================
#  run_all.R  —  single entry point for the EVE/VE preliminary analysis.
#  Runs the pipeline in order: config -> import/validate -> build cohort ->
#  analysis -> report. Inputs come from the local data/ folder (see data/README.md)
#  or from paths set in config.local.R.
#
#  Usage (from the project root):
#     "C:/Program Files/R/R-4.6.0/bin/Rscript.exe" run_all.R
# =============================================================================
PROJ <- tryCatch(dirname(normalizePath(sub("^--file=", "",
          grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), error = function(e) getwd())
if (is.na(PROJ) || !nzchar(PROJ)) PROJ <- getwd()

STEPS <- c("R/00_config.R", "R/01_import_validate.R", "R/02_build_cohort.R", "R/03_analysis.R", "R/04_report.R", "R/05_slides.R")
for (s in STEPS) { cat("\n========== ", s, " ==========\n"); source(file.path(PROJ, s), local = FALSE) }
cat(sprintf("\n[done] clean dataset: %s\n[done] outputs (tables/figures/report): %s\n", CODED_OUT, OUT_DIR))
