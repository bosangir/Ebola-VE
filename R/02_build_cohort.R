# =============================================================================
#  02_build_cohort.R  —  build the CLEAN analysis dataset (one row per participant,
#  coded variables + data dictionary) from the linkage + triangulated lab register,
#  then derive the study-flow table (PowerPoint slide 6). Cleaning & derivation
#  rules follow the DQA / SOP / data dictionary.
#
#  IN  (from 01): LINK, WIDE   OUT: CODED_OUT (Distinct_CODED.csv) + .xlsx + dictionary
#                 + outputs/cohort_flow.csv  + flags on the coded rows
# =============================================================================
if (!exists("LINK") || !exists("WIDE")) source(file.path(PROJ, "R", "01_import_validate.R"), local = FALSE)

## ---- optional: apply dashboard data-cleaning (deletions + consolidated edits) ----
rj  <- function(p) if (file.exists(p)) tryCatch(jsonlite::fromJSON(paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = ""), simplifyVector = FALSE), error = function(e) NULL) else NULL
nId <- function(s) gsub("[^A-Z0-9]", "", toupper(blank(s)))
m <- LINK
if (nzchar(DASH_DATA) && requireNamespace("jsonlite", quietly = TRUE)) {
  del  <- rj(file.path(DASH_DATA, "deletions.json"));            del  <- if (is.null(del))  character(0) else unlist(del)
  corr <- rj(file.path(DASH_DATA, "record_corrections.json"));  corr <- if (is.null(corr)) list()        else corr
  if (length(del)) { keep <- !(g(m, "lab_sample_id") %in% del)
    cat(sprintf("[02] cleaning: dropped %d deleted lab record(s)\n", sum(!keep))); m <- m[keep, , drop = FALSE] }
  if (length(corr)) { labnm <- if ("labreg_name" %in% names(m)) g(m, "labreg_name") else g(m, "NAMES")
    rid <- paste0("WP|", g(m, "lab_sample_id"), "|", nId(paste0(g(m,"name_4"), g(m,"name_5"))), "|", nId(labnm)); nap <- 0
    for (i in seq_len(nrow(m))) { ce <- corr[[rid[i]]]; if (is.null(ce)) next
      for (k in names(ce)) { v <- ce[[k]]; if (k %in% names(m) && length(v) && nzchar(as.character(v)[1])) { m[i, k] <- as.character(v)[1]; nap <- nap + 1 } } }
    if (nap) cat(sprintf("[02] cleaning: applied %d consolidated field value(s)\n", nap)) }
}

## ---- de-duplicate to one row per participant (canonical key; keep within-pair) ----
N_EXPORT <- nrow(m)
m$pk   <- pkey(m$name_1, m$name_4, m$name_5)
m$prio <- ifelse(g(m, "match_type") == "within-pair", 0, 1)
m <- m[order(m$pk, m$prio), ]
mb <- m[!duplicated(m$pk), ]
cat(sprintf("[02] linkage export rows = %d -> distinct participants = %d (%d duplicates consolidated)\n",
            N_EXPORT, nrow(mb), N_EXPORT - nrow(mb)))

## ---- join wide lab + per-participant RT-PCR history (uid-keyed) ----
Wi <- match(g(mb, "lab_sample_id"), g(WIDE, "patient_uid"))
wv <- function(c) if (c %in% names(WIDE)) blank(WIDE[[c]])[Wi] else rep("", nrow(mb))
resCols <- grep("^final_result_evd_[0-9]+$", names(WIDE), value = TRUE)
resN <- as.integer(sub(".*_", "", resCols)); resCols <- resCols[order(resN)]; resN <- sort(resN)
onset_mb <- as.Date(pdate(g(mb, "date_frist_sympt")))
labhist <- function(i){ wr <- Wi[i]; if (is.na(wr)) return(NULL)
  res <- blank(unlist(WIDE[wr, resCols]))
  dt  <- as.Date(pdate(vapply(resN, function(k){ cc <- paste0("test_result_date_", k); if (cc %in% names(WIDE)) blank(WIDE[[cc]][wr]) else "" }, character(1))))
  keep <- res %in% c("Positive","Negative"); if (!any(keep)) return(NULL)
  res <- res[keep]; dt <- dt[keep]; o <- order(is.na(dt), dt); res <- res[o]; dt <- dt[o]; pos <- res == "Positive"
  ons <- onset_mb[i]; epi <- if (!is.na(ons)) which(!is.na(dt) & dt >= ons)[1] else NA_integer_; if (is.na(epi)) epi <- 1L
  list(n = length(res), first_r = res[1], first_d = dt[1], last_r = res[length(res)], last_d = dt[length(dt)],
       epi_r = res[epi], epi_d = dt[epi], mid = if (length(res) > 2) paste(res[2:(length(res)-1)], collapse = ";") else "",
       traj = paste(substr(res, 1, 1), collapse = ""), any_pos = any(pos),
       dpos1 = if (any(pos)) dt[which(pos)[1]] else as.Date(NA), dposL = if (any(pos)) dt[rev(which(pos))[1]] else as.Date(NA)) }
H  <- lapply(seq_len(nrow(mb)), labhist)
gv <- function(f, d = "") vapply(H, function(h){ if (is.null(h) || is.null(h[[f]]) || all(is.na(h[[f]]))) return(d); v <- h[[f]]; if (inherits(v, "Date")) as.character(v) else as.character(v) }, character(1))

## ---- coded variables (demographics, vaccination, lab outcome, vital, exposure meta) ----
age_surv <- numv(g(mb, "age_years")); age_lab <- numv(wv("age_years"))
sex_surv_c <- g(mb, "sex"); sex_lab_raw <- toupper(wv("sex"))
sex_lab_c <- ifelse(substr(sex_lab_raw, 1, 1) == "M", "1", ifelse(substr(sex_lab_raw, 1, 1) == "F", "2", ""))
sex_res_c <- ifelse(nzchar(sex_surv_c) & sex_surv_c %in% c("1","2"), sex_surv_c, sex_lab_c)
age_res   <- ifelse(!is.na(age_surv), age_surv, age_lab)
n_doses   <- (nzchar(pdate(g(mb,"date_vaccination1")))) + (nzchar(pdate(g(mb,"date_vaccination2"))))
n_doses[g(mb,"vaccin_mve") == "1" & n_doses == 0] <- NA
## ---- fallback: single result from the linkage file where there is no uid-keyed
##      specimen history (i.e. no wide lab register). Treated as a one-test history. ----
.res_cands <- c("episode_result","FINAL RESULT (EVD)2","evd_result_final",
                "final_result","pcr_result","pcr_test_result","FINAL RESULT (EVD)")
.rescol <- .res_cands[.res_cands %in% names(mb)][1]
.single <- if (is.na(.rescol)) rep("", nrow(mb)) else {
  .v <- norm(mb[[.rescol]])                       # uppercase + accent-fold (Positif/Négatif ok)
  ifelse(grepl("^POS", .v) | .v == "1", "Positive",
         ifelse(grepl("^NEG", .v) | .v == "0", "Negative", "")) }
.nohist <- gv("n") == ""                           # no wide-lab specimen history for this participant

n_tests <- gv("n");      n_tests[.nohist & nzchar(.single)] <- "1"
first_r <- gv("first_r"); first_r[.nohist] <- .single[.nohist]
last_r  <- gv("last_r");  last_r[.nohist]  <- .single[.nohist]
first_d <- gv("first_d"); last_d <- gv("last_d"); epi_d <- gv("epi_d")   # dates unknown for single-result rows
anyp    <- gv("any_pos")
anyp[.nohist] <- ifelse(.single[.nohist] == "Positive","TRUE",
                 ifelse(.single[.nohist] == "Negative","FALSE",""))
epi_r <- gv("epi_r"); epi_r[.nohist] <- .single[.nohist]
epi_code  <- ifelse(epi_r == "Positive","1", ifelse(epi_r == "Negative","0",""))
case_conf <- ifelse(anyp == "TRUE","1", ifelse(n_tests == "","","0"))
surv_out <- unname(VIT[g(mb,"outcome")]); lab_fs <- wv("final_status")
lab_dead <- grepl("dead|deced|deces|decede", lab_fs, ignore.case = TRUE); lab_alive <- grepl("alive|vivant", lab_fs, ignore.case = TRUE)
vital <- ifelse((surv_out %in% "Dead") | lab_dead, "Dead", ifelse((surv_out %in% "Alive") | lab_alive, "Alive", "Unknown"))

D <- data.frame(
  study_id = sprintf("EVE-%04d", seq_len(nrow(mb))), record_id = g(mb,"name_1"),
  lab_patient_uid = g(mb,"lab_sample_id"), firstname = g(mb,"name_5"), surname = g(mb,"name_4"),
  age_surv = age_surv, age_lab = age_lab, age = age_res,
  age_group = cut(age_res, AGE_BREAKS, labels = AGE_LABELS), age_group_code = as.integer(cut(age_res, AGE_BREAKS)),
  sex_surv = unname(SEX[sex_surv_c]), sex_surv_code = sex_surv_c, sex_lab = unname(SEX[sex_lab_c]), sex_lab_code = sex_lab_c,
  sex = unname(SEX[sex_res_c]), sex_code = sex_res_c, sex_agreement = g(mb,"sex_agreement"),
  province = g(mb,"province_1"), health_zone = g(mb,"health_zone_1"), health_area = g(mb,"health_area_1"), village = g(mb,"village_residence"),
  health_zone_lab = wv("health_zone"), health_area_lab = wv("health_area"),
  occupation_code = g(mb,"occupation"), hcw_frontline_role = g(mb,"type_personnel"),
  hcw_frontline = ifelse(nzchar(g(mb,"type_personnel")) & toupper(g(mb,"type_personnel")) != "RAS","Yes","Unknown"),
  hcw_frontline_code = ifelse(nzchar(g(mb,"type_personnel")) & toupper(g(mb,"type_personnel")) != "RAS","1","99"),
  prior_ebod = unname(YN[g(mb,"infection")]), prior_ebod_code = g(mb,"infection"),
  vacc_status = unname(VAC[g(mb,"vaccin_mve")]), vacc_status_code = g(mb,"vaccin_mve"),
  vacc_product = unname(TYP[g(mb,"type_vaccin1")]), vacc_product_code = g(mb,"type_vaccin1"), n_doses = n_doses,
  vacc_date1 = pdate(g(mb,"date_vaccination1")), vacc_date2 = pdate(g(mb,"date_vaccination2")),
  vacc_source_code = g(mb,"source_vacccination"), vacc_strategy_code = g(mb,"vaccine_strategy"),
  vacc_card = unname(YN[g(mb,"carte")]), vacc_card_code = g(mb,"carte"),
  onset_date = pdate(g(mb,"date_frist_sympt")),
  specimen_date_first = ifelse(nzchar(first_d), first_d, pdate(g(mb,"specimen_collection_date"))),
  n_tests = n_tests, first_test_date = first_d, first_result = first_r,
  first_result_code = ifelse(first_r == "Positive","1", ifelse(first_r == "Negative","0","")),
  intermediate_results = gv("mid"), last_test_date = last_d, last_result = last_r,
  last_result_code = ifelse(last_r == "Positive","1", ifelse(last_r == "Negative","0","")),
  episode_test_date = epi_d, episode_result = epi_r, episode_result_code = epi_code,
  any_positive = ifelse(anyp == "TRUE","Yes", ifelse(n_tests == "","","No")),
  date_first_positive = gv("dpos1"), date_last_positive = gv("dposL"), result_trajectory = gv("traj"),
  pcr_result_patient = wv("pcr_test_result"),
  case_confirmed = ifelse(case_conf == "1","Confirmed case (PCR+)", ifelse(case_conf == "0","Test-negative control (PCR-)","Indeterminate/none")),
  case_confirmed_code = case_conf, orthoebolavirus_species = "",
  vital_status = vital, vital_status_code = ifelse(vital == "Alive","1", ifelse(vital == "Dead","2","99")),
  date_of_death = pdate(wv("date_of_death")),
  ever_hospitalised = g(mb,"ever_hospitalised"), n_hospitalisations = g(mb,"n_hospitalisations"),
  contact_known = unname(YN[g(mb,"contact_connu")]), contact_known_code = g(mb,"contact_connu"),
  contact_with_case = unname(YN[g(mb,"contact")]), contact_with_case_code = g(mb,"contact"),
  funeral_participation = unname(YN[g(mb,"funerailles")]), funeral_code = g(mb,"funerailles"),
  travel = unname(YN[g(mb,"voyage")]), travel_code = g(mb,"voyage"),
  linkage_tier = g(mb,"linkage_tier"), linkage_final_status = g(mb,"final_status"),
  match_type = g(mb,"match_type"),
  is_cross_record = ifelse(g(mb,"match_type") == "cross-record" | grepl("Cross", g(mb,"final_status")),"Yes","No"),
  age_diff = g(mb,"age_diff"), stringsAsFactors = FALSE, check.names = FALSE)

## ---- optional: site-verified RT-PCR result overrides (uid-keyed) win ----
if (nzchar(DASH_DATA) && requireNamespace("jsonlite", quietly = TRUE)) {
  ROV <- rj(file.path(DASH_DATA, "result_overrides.json")); if (is.null(ROV)) ROV <- list()
  ov <- vapply(D$lab_patient_uid, function(u){ v <- ROV[[u]]; if (is.null(v)) "" else as.character(v)[1] }, character(1))
  if (sum(nzchar(ov))) {
    D$episode_result[ov=="Positive"] <- "Positive"; D$any_positive[ov=="Positive"] <- "Yes"
    D$episode_result[ov=="Negative"] <- "Negative"; D$any_positive[ov=="Negative"] <- "No"
    D$episode_result[ov=="Invalid"]  <- "";          D$any_positive[ov=="Invalid"]  <- ""
    D$episode_result_code <- ifelse(D$episode_result=="Positive","1", ifelse(D$episode_result=="Negative","0",""))
    D$case_confirmed_code <- ifelse(D$any_positive=="Yes","1", ifelse(D$any_positive=="No","0", D$case_confirmed_code))
    cat(sprintf("[02] applied %d site-verified result override(s)\n", sum(nzchar(ov))))
  }
  D$result_overridden <- ifelse(nzchar(ov), ov, "")
} else D$result_overridden <- ""
.fct <- vapply(D, is.factor, logical(1)); D[.fct] <- lapply(D[.fct], as.character)   # avoid factor-level NA warnings
D[is.na(D)] <- ""

## ---- write the clean coded dataset + data dictionary ----
source(file.path(PROJ, "R", "data_dictionary.R"), local = TRUE)   # builds DICT for D
dir.create(dirname(CODED_OUT), showWarnings = FALSE, recursive = TRUE)
write.csv(D,    CODED_OUT, row.names = FALSE, na = "")
write.csv(DICT, file.path(OUT_DIR, "Distinct_CODED_data_dictionary.csv"), row.names = FALSE, na = "")
writexl::write_xlsx(list(Distinct_CODED = D, data_dictionary = DICT), sub("\\.csv$", ".xlsx", CODED_OUT))

## ---- study-flow table (PowerPoint slide 6) ----
elig  <- D$episode_result_code %in% c("0","1")           # definitive RT-PCR
kvax  <- D$vacc_status_code %in% c("1","2")              # known vaccination status
onset_ok <- nzchar(D$onset_date)                         # calendar-time covariate available
prim  <- elig & kvax
flow <- data.frame(check.names = FALSE, Step = c(
  "Linkage export (records)", "  duplicates consolidated", "Distinct participants",
  "  within-pair confirmed links", "  cross-record (orphan) links",
  "No definitive RT-PCR (indeterminate/missing)", "Episode-defining RT-PCR (eligible)",
  "  cases (PCR+)", "  controls (PCR-)", "Unknown vaccination status (excluded)",
  "PRIMARY ANALYTIC SAMPLE (definitive result + known vaccination)",
  "  cases / controls", "Missing covariate (onset -> calendar time)", "ADJUSTED MODEL sample"),
  N = c(N_EXPORT, N_EXPORT - nrow(D), nrow(D),
        sum(D$is_cross_record != "Yes"), sum(D$is_cross_record == "Yes"),
        sum(!elig), sum(elig), sum(elig & D$episode_result_code=="1"), sum(elig & D$episode_result_code=="0"),
        sum(elig & !kvax), sum(prim),
        NA, sum(prim & !onset_ok), sum(prim & onset_ok)))
flow$N <- ifelse(is.na(flow$N),
                 sprintf("%d / %d", sum(prim & D$episode_result_code=="1"), sum(prim & D$episode_result_code=="0")),
                 as.character(flow$N))
write.csv(flow, file.path(OUT_DIR, "cohort_flow.csv"), row.names = FALSE, na = "")

## ---- clean master workbook: the de-duplicated participants, dictionary and flow ----
master_xlsx <- file.path(OUT_DIR, sprintf("EVE_Master_Distinct_%d_cleaned.xlsx", nrow(D)))
writexl::write_xlsx(list(Participants = D, Data_dictionary = DICT, Study_flow = flow), master_xlsx)
cat(sprintf("[02] clean master (deduplicated, %d participants) -> %s\n", nrow(D), master_xlsx))

cat(sprintf("[02] coded dataset: %d participants x %d variables -> %s\n", nrow(D), ncol(D), CODED_OUT))
cat(sprintf("[02] flow: export %d -> distinct %d -> eligible %d (%d cases/%d controls) -> primary %d\n",
            N_EXPORT, nrow(D), sum(elig), sum(elig & D$episode_result_code=="1"), sum(elig & D$episode_result_code=="0"), sum(prim)))
