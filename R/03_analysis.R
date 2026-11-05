# =============================================================================
#  03_analysis.R  —  Vaccine-effectiveness analysis (test-negative design).
#
#  Reads : outputs/Distinct_CODED.csv  (one row per participant, from 02)
#  Writes: outputs/results_bundle.rds   (all results, read by the report)
#          outputs/analysis_tables.xlsx (every table, one per sheet)
#          outputs/figures/F1..F5.png
#
#  Read it top to bottom. Sections:
#    1  Load the coded data
#    2  Build the analysis variables (outcome Y, exposure V, covariates)
#    3  Small helper functions (logistic fit, Firth fit, table row)
#    4  PRIMARY VE: crude, adjusted, SAP-adjusted + sensitivity models
#    5  PROTOCOL WINDOW: the >10-day "effectively vaccinated" group
#    6  PRIMARY 2: VE against death among confirmed cases
#    7  Descriptive tables (characteristics, completeness, timing, DQ)
#    8  SAP confounder coverage (what is / isn't in the adjusted model)
#    9  Figures F1-F5
#    10 Save everything
# =============================================================================

# ---- 1. Load the coded dataset ----------------------------------------------
D <- read.csv(CODED_OUT, colClasses = "character", check.names = FALSE,
              na.strings = character(0), encoding = "UTF-8")
D[is.na(D)] <- ""

# ---- 2. Analysis variables --------------------------------------------------
# numbers and dates
D$age     <- numv(D$age)
D$onset   <- asDate(D$onset_date)
D$vacc_dt <- asDate(D$vacc_date1)

# outcome Y: 1 = PCR positive (case), 0 = negative (control), NA = indeterminate
D$Y_episode <- ifelse(D$episode_result_code == "1", 1L, ifelse(D$episode_result_code == "0", 0L, NA_integer_))
D$Y_first   <- ifelse(D$first_result_code  == "1", 1L, ifelse(D$first_result_code  == "0", 0L, NA_integer_))
D$Y_last    <- ifelse(D$last_result_code   == "1", 1L, ifelse(D$last_result_code   == "0", 0L, NA_integer_))
D$Y_any     <- ifelse(D$any_positive == "Yes", 1L, ifelse(D$any_positive == "No", 0L, NA_integer_))

# exposure V: 1 = vaccinated, 0 = not vaccinated, NA = unknown (never imputed)
D$V_status <- ifelse(D$vacc_status_code == "1", 1L, ifelse(D$vacc_status_code == "2", 0L, NA_integer_))

# covariates for adjustment
D$agegrp  <- cut(D$age, breaks = AGE_BREAKS, labels = AGE_LABELS)
D$sex_f   <- factor(ifelse(D$sex %in% c("Male","Female"), D$sex, NA), levels = c("Female","Male"))
D$caltime <- as.numeric(D$onset - suppressWarnings(min(D$onset, na.rm = TRUE))) / 7   # weeks since first onset

# timing checks
D$vax_to_onset  <- as.numeric(D$onset - D$vacc_dt)
D$n_tests_n     <- suppressWarnings(as.integer(D$n_tests)); D$n_tests_n[is.na(D$n_tests_n)] <- 0L
D$specimen_date <- asDate(D$episode_test_date)
D$onset_to_swab <- as.numeric(D$specimen_date - D$onset)

# protocol exposure: effective only if vaccinated > 10 days before onset
D$exposure <- dplyr::case_when(
  D$vacc_status == "Not vaccinated"                                                              ~ "Unvaccinated",
  D$vacc_status == "Vaccinated" & !is.na(D$vax_to_onset) & D$vax_to_onset >  PROTOCOL_WINDOW_DAYS ~ "Vaccinated >10d (effective)",
  D$vacc_status == "Vaccinated" & !is.na(D$vax_to_onset) & D$vax_to_onset >= 0 & D$vax_to_onset <= PROTOCOL_WINDOW_DAYS ~ "Vaccinated 0-10d (not yet protected)",
  D$vacc_status == "Vaccinated" & !is.na(D$vax_to_onset) & D$vax_to_onset <  0 ~ "Vaccinated after onset (ineligible)",
  D$vacc_status == "Vaccinated" & is.na(D$vax_to_onset)                        ~ "Vaccinated, timing unknown",
  TRUE                                                                         ~ "Unknown status")
D$V_protocol <- ifelse(D$exposure == "Vaccinated >10d (effective)", 1L, ifelse(D$exposure == "Unvaccinated", 0L, NA_integer_))
D$V_early    <- ifelse(D$exposure == "Vaccinated 0-10d (not yet protected)", 1L, ifelse(D$exposure == "Unvaccinated", 0L, NA_integer_))
D$V_reclass  <- ifelse(D$exposure == "Vaccinated >10d (effective)", 1L, ifelse(D$exposure %in% c("Unvaccinated","Vaccinated after onset (ineligible)"), 0L, NA_integer_))

# linkage tier label + RT-PCR result transition (first -> last)
D$link_method <- ifelse(grepl("Tier 1", D$linkage_tier), "Automatic / exact (Tier 1)",
                 ifelse(grepl("Tier 2", D$linkage_tier), "Probabilistic (Tier 2)", "Manual / adjudicated (Tier 3-4 / cross-record)"))
D$transition <- with(D, ifelse(n_tests_n < 2, ifelse(first_result == "", "", paste0("Single (", substr(first_result,1,1), ")")),
  ifelse(first_result=="Negative" & last_result=="Positive","N->P",
  ifelse(first_result=="Positive" & last_result=="Negative","P->N",
  ifelse(first_result=="Negative" & last_result=="Negative","N->N",
  ifelse(first_result=="Positive" & last_result=="Positive","P->P","other"))))))

# exposure-risk and clinical variables (Yes/No/Unknown; "Unknown" kept as a level)
col <- function(v) if (v %in% names(D)) D[[v]] else rep("", nrow(D))
D$hosp      <- yn(col("ever_hospitalised")); D$n_hosp <- numv(col("n_hospitalisations"))
D$contact_k <- yn(col("contact_known_code")); D$contact_c <- yn(col("contact_with_case_code"))
D$funeral   <- yn(col("funeral_code")); D$travel <- yn(col("travel_code"))
D$vcard     <- yn(col("vacc_card_code")); D$prior <- yn(col("prior_ebod_code"))
D$hcw       <- ifelse(col("hcw_frontline") == "Yes","Yes","Unknown")
D$died      <- ifelse(D$vital_status == "Dead","Yes", ifelse(D$vital_status == "Alive","No","Unknown"))
D$contact_adj <- factor(D$contact_c, levels = c("No","Yes","Unknown"))
D$funeral_adj <- factor(D$funeral,   levels = c("No","Yes","Unknown"))

# adjustment sets: ADJ_SLIDE = the October slides; ADJ_SAP = + exposure risk (SAP emphasis)
ADJ_SLIDE <- c("agegrp","sex_f","caltime")
ADJ_SAP   <- c("agegrp","sex_f","caltime","contact_adj","funeral_adj")

# handy subsets
Dp <- D[D$is_cross_record != "Yes", , drop = FALSE]                 # within-pair only (sensitivity)
cc <- D[!is.na(D$Y_episode), ]; cc$grp <- ifelse(cc$Y_episode == 1, "Case (PCR+)", "Control (PCR-)")  # cases + controls

# ---- 3. Helper functions ----------------------------------------------------
# fit(): ordinary logistic regression; returns OR, VE and 95% CI for the exposure.
fit <- function(dat, outcome, exposure = "V_status", adj = NULL){
  keep <- adj[vapply(adj, function(a) length(unique(dat[[a]][!is.na(dat[[a]])])) > 1, logical(1))]  # drop constant covariates
  dat  <- dat[!is.na(dat[[outcome]]) & !is.na(dat[[exposure]]), , drop = FALSE]
  if (length(keep)) dat <- dat[complete.cases(dat[, c(outcome, exposure, keep)]), , drop = FALSE]
  mo <- tryCatch(glm(as.formula(paste(outcome, "~", paste(c(exposure, keep), collapse = " + "))),
                     data = dat, family = binomial()), error = function(e) NULL)
  n <- nrow(dat); cs <- sum(dat[[outcome]] == 1); ct <- sum(dat[[outcome]] == 0)
  if (is.null(mo)) return(list(ok = FALSE, n = n, cases = cs, controls = ct,
                               OR = NA, OR_L = NA, OR_U = NA, VE = NA, VE_L = NA, VE_U = NA, adj = keep))
  ci <- suppressWarnings(tryCatch(confint(mo), error = function(e) confint.default(mo)))
  co <- coef(mo); en <- grep(paste0("^", exposure), names(co), value = TRUE)[1]
  or <- exp(co[en]); lo <- exp(ci[en,1]); hi <- exp(ci[en,2]); ve <- ve_from_or(unname(or), unname(lo), unname(hi))
  list(ok = TRUE, n = n, cases = cs, controls = ct, adj = keep,
       OR = unname(or), OR_L = unname(lo), OR_U = unname(hi), VE = ve["VE"], VE_L = ve["VE_L"], VE_U = ve["VE_U"]) }

# firth()/firth_fit(): penalised logistic regression for sparse data (handles separation).
firth <- function(X, y, mx = 200, tol = 1e-9){ b <- rep(0, ncol(X)); Xi <- diag(ncol(X))
  for (it in 1:mx){ p <- as.vector(plogis(X %*% b)); W <- p*(1-p); Wr <- sqrt(W); Xi <- solve(crossprod(X*Wr))
    Q <- (X*Wr) %*% Xi; h <- rowSums(Q*(X*Wr)); st <- Xi %*% crossprod(X, (y-p) + h*(0.5-p)); b <- b + st; if (max(abs(st)) < tol) break }
  list(coef = as.vector(b), se = sqrt(diag(Xi))) }
firth_fit <- function(dat, rhs, exposure = "V_status", outcome = "Y_episode"){ tryCatch({
    dat <- dat[!is.na(dat[[outcome]]) & complete.cases(dat[, all.vars(as.formula(paste(outcome, "~", rhs)))]), , drop = FALSE]
    mm  <- model.matrix(as.formula(paste("~", rhs)), dat); fr <- firth(mm, dat[[outcome]]); j <- which(colnames(mm) == exposure)
    or  <- exp(fr$coef[j]); lo <- exp(fr$coef[j] - 1.96*fr$se[j]); hi <- exp(fr$coef[j] + 1.96*fr$se[j]); ve <- ve_from_or(or, lo, hi)
    list(OR = or, OR_L = lo, OR_U = hi, VE = ve["VE"], VE_L = ve["VE_L"], VE_U = ve["VE_U"],
         n = nrow(dat), cases = sum(dat[[outcome]]==1), controls = sum(dat[[outcome]]==0))
  }, error = function(e){ message("[03] Firth fit (", rhs, ") skipped: ", conditionMessage(e))
    list(OR=NA, OR_L=NA, OR_U=NA, VE=NA, VE_L=NA, VE_U=NA, n=NA, cases=NA, controls=NA) }) }

# mrow(): one display row (OR, VE, CI) for a model; shows "not estimable" when a fit separates.
mrow <- function(m, name){
  deg <- is.na(m$OR) || !is.finite(m$OR) || m$OR < 1e-4 || m$OR > 1e4 ||
         is.na(m$OR_L) || is.na(m$OR_U) || !is.finite(m$OR_U) || m$OR_U > 1e4 || m$OR_L < 1e-4
  data.frame(Model = name, N = m$n, Cases = m$cases, Controls = m$controls,
    OR = if (deg) "~0 (separation)" else sprintf("%.2f", m$OR),
    `OR 95% CI` = if (deg) "not estimable" else sprintf("%.2f-%.2f", m$OR_L, m$OR_U),
    `VE %` = if (deg) "not estimable" else sprintf("%.1f", m$VE),
    `VE 95% CI` = if (deg) "0 vaccinated cases - use Firth" else sprintf("%.1f to %.1f", m$VE_L, m$VE_U),
    check.names = FALSE) }
# mfp(): one numeric row (VE + CI) for the forest figures.
mfp <- function(m, name, ord) data.frame(model = name, ord = ord, VE = m$VE, VE_L = m$VE_L, VE_U = m$VE_U,
  cases = m$cases, controls = m$controls, n = m$n, check.names = FALSE)

# ---- 4. PRIMARY VE: models + sensitivity ------------------------------------
# produces: crude/adj/adj_sap/crude_ex/fr_* fits, model_tbl (Table), ve_fp (forest)
crude     <- fit(D,  "Y_episode", "V_status")                      # vaccinated vs not, no adjustment
adj       <- fit(D,  "Y_episode", "V_status", ADJ_SLIDE)           # slides: age, sex, calendar time
adj_sap   <- fit(D,  "Y_episode", "V_status", ADJ_SAP)             # SAP: + contact, funeral (exposure risk)
crude_ex  <- fit(Dp, "Y_episode", "V_status")                      # sensitivity: drop cross-record links
fr_crude  <- firth_fit(D, "V_status")                              # Firth (sparse data)
fr_adj    <- firth_fit(D, "V_status + agegrp + sex_f + caltime")
fr_adjsap <- firth_fit(D, "V_status + agegrp + sex_f + caltime + contact_adj + funeral_adj")

model_tbl <- rbind(
  mrow(crude,     "Crude (vaccinated vs unvaccinated)"),
  mrow(adj,       "Adjusted (age, sex, calendar time)"),
  mrow(adj_sap,   "Adjusted + exposure risk (SAP: + contact, funeral)"),
  mrow(crude_ex,  "Crude, excluding cross-record links (sensitivity)"),
  mrow(fr_crude,  "Firth crude (sparse-data sensitivity)"),
  mrow(fr_adj,    "Firth adjusted (age, sex, calendar time)"),
  mrow(fr_adjsap, "Firth adjusted + exposure risk (SAP, sparse-data)"))

ve_fp <- rbind(
  mfp(crude,     "Crude", 1),
  mfp(adj,       "Adjusted (age, sex, cal. time)", 2),
  mfp(adj_sap,   "Adjusted + exposure risk (SAP)", 3),
  mfp(crude_ex,  "Crude, excl. cross-record", 4),
  mfp(fr_crude,  "Firth crude", 5),
  mfp(fr_adj,    "Firth adjusted", 6),
  mfp(fr_adjsap, "Firth adjusted + exposure risk (SAP)", 7))

# outcome-definition sensitivity: episode / first / final / any-positive
outdefs   <- c(`Episode (primary)`="Y_episode", `First result`="Y_first", `Final result`="Y_last", `Any-positive`="Y_any")
sens_fits <- lapply(names(outdefs), function(nm) list(name = nm, oc = outdefs[[nm]], m = fit(D, outdefs[[nm]], "V_status")))
sens <- do.call(rbind, lapply(sens_fits, function(z){ mm <- z$m; d <- D[!is.na(D[[z$oc]]) & !is.na(D$V_status), ]
  data.frame(`Outcome definition`=z$name, N=mm$n, Cases=mm$cases, Controls=mm$controls,
    `Vaccinated cases`=sum(d[[z$oc]]==1 & d$V_status==1), OR=sprintf("%.2f",mm$OR),
    `VE %`=sprintf("%.1f",mm$VE), `VE 95% CI`=sprintf("%.1f to %.1f",mm$VE_L,mm$VE_U), check.names=FALSE) }))
sens_fp <- do.call(rbind, lapply(sens_fits, function(z) data.frame(def = z$name,
  OR = z$m$OR, OR_L = z$m$OR_L, OR_U = z$m$OR_U, VE = z$m$VE, VE_L = z$m$VE_L, VE_U = z$m$VE_U, check.names = FALSE)))

# ---- 5. PROTOCOL WINDOW: the >10-day "effectively vaccinated" group ----------
# No confirmed case is vaccinated >10d before onset, so ordinary models separate;
# we report Firth-penalised estimates and show the case/control split.
# produces: expo_class (who is kept/excluded), extra_models (Table E2), pw_fp (forest)
protocol       <- fit(D, "Y_episode", "V_protocol")                 # crude (separates)
protocol_adjex <- fit(D, "Y_episode", "V_protocol", ADJ_SAP)        # adjusted (separates)
fr_protocol    <- firth_fit(D, "V_protocol", "V_protocol")          # Firth (usable estimate)
early          <- fit(D, "Y_episode", "V_early")
reclass        <- fit(D, "Y_episode", "V_reclass")
fr_early       <- firth_fit(D, "V_early",   "V_early")
fr_reclass     <- firth_fit(D, "V_reclass", "V_reclass")

extra_models <- rbind(
  mrow(protocol,       "Protocol >10d vs unvaccinated (crude)"),
  mrow(fr_protocol,    "Protocol >10d (Firth crude)"),
  mrow(protocol_adjex, "Protocol >10d, adjusted + contact/funeral risk"),
  mrow(early,          "Vaccinated 0-10d (not yet protected) vs unvaccinated"),
  mrow(reclass,        "Vaccinated-after-onset counted as unvaccinated (sensitivity)"))

# who falls in each exposure category, by case/control
expo_lv <- c("Unvaccinated","Vaccinated >10d (effective)","Vaccinated 0-10d (not yet protected)",
             "Vaccinated after onset (ineligible)","Vaccinated, timing unknown","Unknown status")
expo_class <- as.data.frame.matrix(table(factor(cc$exposure, levels = expo_lv), cc$grp))
expo_class <- data.frame(`Exposure category` = rownames(expo_class), expo_class,
  `In protocol analysis` = ifelse(rownames(expo_class) %in% c("Unvaccinated","Vaccinated >10d (effective)"),"Yes",
    ifelse(rownames(expo_class) == "Vaccinated 0-10d (not yet protected)","Separate analysis","Excluded")),
  check.names = FALSE, row.names = NULL)

# forest data: Firth protocol contrasts + status-based for reference
pw_fp <- rbind(
  data.frame(model = "Status-based: crude",                            VE = crude$VE,       VE_L = crude$VE_L,       VE_U = crude$VE_U,       grp = "Status (reference)", ord = 1),
  data.frame(model = "Status-based: Firth crude",                      VE = fr_crude$VE,    VE_L = fr_crude$VE_L,    VE_U = fr_crude$VE_U,    grp = "Status (reference)", ord = 2),
  data.frame(model = "Protocol >10d (effective) – Firth",         VE = fr_protocol$VE, VE_L = fr_protocol$VE_L, VE_U = fr_protocol$VE_U, grp = "Protocol window",    ord = 3),
  data.frame(model = "Vaccinated 0–10d (not protected) – Firth", VE = fr_early$VE, VE_L = fr_early$VE_L,  VE_U = fr_early$VE_U,    grp = "Protocol window",    ord = 4),
  data.frame(model = "After-onset = unvaccinated – Firth",        VE = fr_reclass$VE,  VE_L = fr_reclass$VE_L,  VE_U = fr_reclass$VE_U,  grp = "Sensitivity",        ord = 5))

# ---- 6. PRIMARY 2: VE against death among confirmed cases --------------------
# Cohort = confirmed cases. Outcome = death. Measure = risk ratio; VE_death = (1 - RR) x 100.
# produces: mortality_tbl, mort_counts
CSm      <- D[D$Y_episode %in% 1, , drop = FALSE]
CSm$Died <- ifelse(CSm$vital_status == "Dead", 1L, ifelse(CSm$vital_status == "Alive", 0L, NA_integer_))
mdat     <- CSm[!is.na(CSm$Died) & !is.na(CSm$V_status), , drop = FALSE]

rr_block <- function(d){                                            # risk ratio with Katz log 95% CI
  a <- sum(d$V_status==1 & d$Died==1); nV <- sum(d$V_status==1)
  b <- sum(d$V_status==0 & d$Died==1); nU <- sum(d$V_status==0)
  rV <- if (nV>0) a/nV else NA_real_; rU <- if (nU>0) b/nU else NA_real_; rr <- rV/rU
  if (is.na(rr) || a==0 || b==0) return(list(a=a,nV=nV,b=b,nU=nU,rV=rV,rU=rU,RR=rr,RR_L=NA,RR_U=NA,VE=(1-rr)*100,VE_L=NA,VE_U=NA))
  se <- sqrt((1-rV)/a + (1-rU)/b); lo <- rr*exp(-1.96*se); hi <- rr*exp(1.96*se)
  list(a=a,nV=nV,b=b,nU=nU,rV=rV,rU=rU,RR=rr,RR_L=lo,RR_U=hi,VE=(1-rr)*100,VE_L=(1-hi)*100,VE_U=(1-lo)*100) }
mrr           <- rr_block(mdat)
mort_or_crude <- fit(CSm, "Died", "V_status")
mort_or_adj   <- fit(CSm, "Died", "V_status", c("agegrp","sex_f"))
mort_fr       <- firth_fit(mdat, "V_status", exposure = "V_status", outcome = "Died")

fmtci <- function(l,u,f="%.1f to %.1f") if (is.na(l)||is.na(u)) "undefined (sparse/zero cell)" else sprintf(f,l,u)
mort_counts <- sprintf("Deaths among confirmed cases: vaccinated %d/%d (%.0f%%), not vaccinated %d/%d (%.0f%%); %d case(s) with unknown vaccination or vital status excluded.",
  mrr$a, mrr$nV, 100*ifelse(is.na(mrr$rV),0,mrr$rV), mrr$b, mrr$nU, 100*ifelse(is.na(mrr$rU),0,mrr$rU), sum(CSm$Y_episode==1)-nrow(mdat))
mrowM <- function(measure, est, est_l, est_u, m, estfmt="%.2f"){     # one display row for the mortality table
  estimable <- !(is.na(m$VE_L) || is.na(m$VE_U))
  data.frame(Measure=measure, Estimate=ifelse(is.na(est),"NA",sprintf(estfmt,est)),
    `Estimate 95% CI`=fmtci(est_l,est_u,"%.2f-%.2f"),
    `VE against death %`=if (!estimable) "not estimable" else ifelse(is.na(m$VE),"NA",sprintf("%.1f",m$VE)),
    `VE 95% CI`=fmtci(m$VE_L,m$VE_U), check.names=FALSE) }
mortality_tbl <- rbind(
  mrowM("Crude risk ratio (cohort)",      mrr$RR,           mrr$RR_L,           mrr$RR_U,           mrr),
  mrowM("Crude odds ratio",               mort_or_crude$OR, mort_or_crude$OR_L, mort_or_crude$OR_U, mort_or_crude),
  mrowM("Adjusted odds ratio (age, sex)", mort_or_adj$OR,   mort_or_adj$OR_L,   mort_or_adj$OR_U,   mort_or_adj),
  mrowM("Firth odds ratio (sparse-data)", mort_fr$OR,       mort_fr$OR_L,       mort_fr$OR_U,       mort_fr))

# ---- 7. Descriptive tables --------------------------------------------------
ft <- function(x, lv=NULL){ x <- ifelse(is.na(x)|x=="","Missing",as.character(x)); if (is.null(lv)) lv <- names(sort(table(x),decreasing=TRUE))
  tb <- as.integer(table(factor(x,levels=lv))); data.frame(Category=lv, n=tb, `%`=round(100*tb/sum(tb),1), check.names=FALSE) }
np <- function(n,d) sprintf("%d (%.1f%%)", n, ifelse(d>0,100*n/d,0))

# characteristics of cases vs controls (slide 8)
gg <- split(cc, cc$grp)
char_by_pcr <- data.frame(Variable=c("N","Age median (IQR)","Female n (%)","Vaccinated n (%)","Unknown vacc n (%)","Health zone Bulape n (%)","Died n (%)"), check.names=FALSE)
for (k in names(gg)) { d <- gg[[k]]; char_by_pcr[[k]] <- c(nrow(d),
  sprintf("%.0f (%.0f-%.0f)", median(d$age,na.rm=TRUE), quantile(d$age,.25,na.rm=TRUE), quantile(d$age,.75,na.rm=TRUE)),
  np(sum(d$sex=="Female"),nrow(d)), np(sum(d$vacc_status=="Vaccinated"),nrow(d)), np(sum(d$vacc_status=="Unknown"),nrow(d)),
  np(sum(d$health_zone=="Bulape"),nrow(d)), np(sum(d$vital_status=="Dead"),nrow(d))) }

# missing-data rate (slide 7); vaccination date/strategy are among the vaccinated only
nv    <- sum(D$vacc_status=="Vaccinated")
compl <- function(k,den) sprintf("%.1f%%", ifelse(den>0,100*(den-k)/den,0))
completeness <- data.frame(Variable=c("RT-PCR result (episode)","Symptom-onset date","Vaccination status","Vaccination date (among vaccinated)","Vaccination strategy (among vaccinated)","Age","Sex","Health zone","Village","Vital status"),
  `Missing %`=c(compl(sum(!is.na(D$Y_episode)),nrow(D)), compl(sum(!is.na(D$onset)),nrow(D)), compl(sum(D$vacc_status_code %in% c("1","2")),nrow(D)),
    compl(sum(D$vacc_status=="Vaccinated" & nzchar(D$vacc_date1)),nv), compl(sum(D$vacc_status=="Vaccinated" & nzchar(D$vacc_strategy_code)),nv),
    compl(sum(nzchar(D$age)),nrow(D)), compl(sum(D$sex %in% c("Male","Female")),nrow(D)), compl(sum(nzchar(D$health_zone)),nrow(D)),
    compl(sum(nzchar(D$village)),nrow(D)), compl(sum(D$vital_status %in% c("Alive","Dead")),nrow(D))), check.names=FALSE)

# vaccination timing (slide 9) and RT-PCR transitions (slide 13)
vacc_timing <- ft(D$exposure[D$vacc_status=="Vaccinated"])
rep_tested  <- D[D$n_tests_n>=2, ]; trans_tab <- ft(rep_tested$transition, c("N->N","N->P","P->P","P->N","other"))

# data-quality flags (slide 14)
dq <- data.frame(Check=c("Onset-to-swab > 30 days","Vaccinated after symptom onset","Vaccinated, vaccination date missing","Linked, RT-PCR indeterminate/missing","Cross-record (Tier 3/4) links in primary","Specimen before symptom onset"),
  N=c(sum(D$onset_to_swab>30,na.rm=TRUE), sum(D$vax_to_onset<0,na.rm=TRUE), sum(D$vacc_status=="Vaccinated" & is.na(D$vacc_dt)),
      sum(is.na(D$Y_episode)), sum(D$is_cross_record=="Yes"), sum(D$onset_to_swab<0,na.rm=TRUE)), check.names=FALSE)

# exposure-risk & clinical characteristics by case/control, with Fisher tests
pf <- function(p) ifelse(is.na(p),"", ifelse(p < 0.001,"<0.001", sprintf("%.3f", p)))
cat_row <- function(var, label, levels = c("Yes","No","Unknown")){ x <- cc[[var]]; gcol <- cc$grp; G <- sort(unique(gcol))
  k <- x != "Unknown"; tb <- table(factor(x[k], levels = setdiff(levels,"Unknown")), gcol[k]); tb <- tb[rowSums(tb) > 0, , drop = FALSE]
  p <- if (nrow(tb) >= 2 && ncol(tb) >= 2) tryCatch(fisher.test(tb)$p.value, error = function(e) NA) else NA
  out <- data.frame(Variable = c(label, paste0("    ", levels)), check.names = FALSE)
  for (gg2 in G) { d <- x[gcol == gg2]; tt <- as.integer(table(factor(d, levels = levels))); out[[gg2]] <- c("", sprintf("%d (%.1f)", tt, 100 * tt / max(1, length(d)))) }
  out$`p-value` <- c(pf(p), rep("", length(levels))); out }
extra_char <- do.call(rbind, list(
  cat_row("hosp","Ever hospitalised"), cat_row("died","Died"),
  cat_row("contact_k","Known contact of a case"), cat_row("contact_c","Contact with a confirmed case"),
  cat_row("funeral","Funeral / burial participation"), cat_row("travel","Travel history"),
  cat_row("hcw","Health / frontline worker", c("Yes","Unknown")), cat_row("prior","Prior documented Ebola disease")))

# completeness of the exposure-risk variables
extra_complete <- do.call(rbind, lapply(list(c("hosp","Ever hospitalised"), c("contact_k","Known contact of a case"),
  c("contact_c","Contact with a confirmed case"), c("funeral","Funeral / burial participation"),
  c("travel","Travel history"), c("vcard","Vaccination card seen")), function(v){ k <- sum(D[[v[1]]] != "Unknown")
  data.frame(Variable = v[2], `Known n (%)` = sprintf("%d (%.1f)", k, 100*k/nrow(D)),
             `Unknown n (%)` = sprintf("%d (%.1f)", nrow(D)-k, 100*(nrow(D)-k)/nrow(D)), check.names = FALSE) }))

# severe outcomes among confirmed cases (descriptive only)
sev_block <- function(expvar, label){ dd <- CSm[!is.na(CSm[[expvar]]), ]; if (!nrow(dd)) return(NULL); dd$g <- ifelse(dd[[expvar]] == 1,"Vaccinated","Unvaccinated")
  do.call(rbind, lapply(c("died","hosp"), function(v){ k <- dd[[v]] != "Unknown"
    tb <- table(factor(dd$g[k], levels = c("Vaccinated","Unvaccinated")), factor(dd[[v]][k], levels = c("Yes","No")))
    p <- tryCatch(fisher.test(tb)$p.value, error = function(e) NA)
    data.frame(Exposure = label, Outcome = ifelse(v == "died","Died","Ever hospitalised"),
      `Vaccinated n/N (%)`   = sprintf("%d/%d (%.1f)", tb["Vaccinated","Yes"],   sum(tb["Vaccinated",]),   100*tb["Vaccinated","Yes"]/max(1, sum(tb["Vaccinated",]))),
      `Unvaccinated n/N (%)` = sprintf("%d/%d (%.1f)", tb["Unvaccinated","Yes"], sum(tb["Unvaccinated",]), 100*tb["Unvaccinated","Yes"]/max(1, sum(tb["Unvaccinated",]))),
      `Fisher p` = pf(p), check.names = FALSE) })) }
severity_cases <- rbind(sev_block("V_protocol","Protocol (>10d vs unvaccinated)"), sev_block("V_status","Status-based (any vaccination)"))

# ---- 8. SAP confounder coverage ---------------------------------------------
# produces: sap_confounders (which a-priori confounder is in the model, and why)
pc <- function(ok) sprintf("%.0f%%", 100*mean(ok))
sap_confounders <- data.frame(check.names = FALSE,
  `SAP a-priori confounder` = c("Age","Sex","Calendar time of testing","Geographic area (health zone)",
    "Healthcare / frontline worker status","Contact with confirmed case (exposure risk)",
    "Funeral / burial participation (exposure risk)","Travel history","Prior documented EBOD",
    "Testing / ETU site","Outbreak"),
  `Known in data` = c(pc(!is.na(D$age)), pc(D$sex %in% c("Male","Female")), pc(!is.na(D$onset)),
    pc(nzchar(D$health_zone)), pc(D$hcw=="Yes"), pc(D$contact_c!="Unknown"), pc(D$funeral!="Unknown"),
    pc(D$travel!="Unknown"), pc(D$prior!="Unknown"), "0%", "single outbreak"),
  `In adjusted model?` = c("Yes","Yes","Yes",
    sprintf("No - near-constant (%s Bulape)", pc(D$health_zone=="Bulape")),
    "No - mostly unknown","Yes (SAP model)","Yes (SAP model)",
    "Exploratory only","Exploratory only","No - not recorded","No - single outbreak"))

# ---- 9. Figures F1-F5 -------------------------------------------------------
thm <- theme_minimal(base_size=11) + theme(panel.grid.minor=element_blank(), plot.title=element_text(face="bold"))
ggs <- function(f,p,w=7,h=4) suppressWarnings(ggsave(file.path(OUT_DIR,"figures",f), p, width=w, height=h, dpi=150, bg="white"))

# F1: vaccination-to-onset interval (dashed line = 10-day protocol window)
try(ggs("F1_vacc_to_onset.png", ggplot(D[!is.na(D$vax_to_onset),], aes(vax_to_onset)) +
  geom_histogram(binwidth=5, fill="#2C7FB8", colour="white") +
  geom_vline(xintercept=PROTOCOL_WINDOW_DAYS, linetype="dashed", colour="#C62828") +
  labs(title="Vaccination-to-onset interval", subtitle="dashed = 10-day window", x="Days vaccination to onset", y="Participants") + thm), silent=TRUE)

# F2: RT-PCR result transitions (participants with >=2 specimens)
tt2 <- trans_tab[trans_tab$n>0,]
if (nrow(tt2)) try(ggs("F2_transitions.png", ggplot(tt2, aes(reorder(Category,n),n)) +
  geom_col(fill="#756BB1") + coord_flip() + labs(title="RT-PCR transitions (>=2 specimens)", x=NULL, y="Participants") + thm), silent=TRUE)

# F3: headline VE by model with 95% CI (green = CI excludes 0)
vf <- ve_fp[!is.na(ve_fp$VE), , drop = FALSE]
vf$model <- factor(vf$model, levels = rev(vf$model[order(vf$ord)]))
vf$sig   <- ifelse(!is.na(vf$VE_L) & vf$VE_L > 0, "95% CI excludes 0 (significant)", "95% CI includes 0 (not significant)")
vf$lab   <- ifelse(is.na(vf$VE), "", sprintf("%.0f%%", vf$VE))
try(ggs("F3_VE_by_model.png", ggplot(vf, aes(VE, model, colour = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_errorbarh(aes(xmin = pmax(VE_L, -100), xmax = pmin(VE_U, 100)), height = 0.22, linewidth = 0.7, na.rm = TRUE) +
  geom_point(size = 3, na.rm = TRUE) + geom_text(aes(label = lab), vjust = -0.9, size = 3.2, show.legend = FALSE, na.rm = TRUE) +
  scale_colour_manual(values = c("95% CI excludes 0 (significant)" = "#1B7837", "95% CI includes 0 (not significant)" = "#B2182B"), name = NULL) +
  coord_cartesian(xlim = c(-100, 100)) +
  labs(title = "Vaccine effectiveness by model", subtitle = "VE = (1 - OR) x 100; bars = 95% CI (clamped to +/-100)", x = "Vaccine effectiveness (%)", y = NULL) +
  thm + theme(legend.position = "bottom"), w = 8, h = 4.6), silent = TRUE)

# F4: odds ratio by RT-PCR outcome definition with 95% CI
of <- sens_fp[!is.na(sens_fp$OR), , drop = FALSE]; of$def <- factor(of$def, levels = rev(of$def))
try(ggs("F4_outcome_def_forest.png", ggplot(of, aes(OR, def)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
  geom_errorbarh(aes(xmin = OR_L, xmax = OR_U), height = 0.2, colour = "#2C3E82", na.rm = TRUE) +
  geom_point(size = 2.8, colour = "#2C3E82", na.rm = TRUE) + scale_x_log10() +
  labs(title = "Odds ratio by RT-PCR outcome definition", subtitle = "OR<1 favours protection; bars = 95% CI", x = "OR (log scale)", y = NULL) + thm), silent = TRUE)

# F5: protocol-window VE (Firth); every vaccinated-timing subgroup has 0 cases
pw <- pw_fp[is.finite(pw_fp$VE) & is.finite(pw_fp$VE_L) & is.finite(pw_fp$VE_U) & pw_fp$VE_L > -500, , drop = FALSE]
if (nrow(pw)) { pw$model <- factor(pw$model, levels = rev(pw$model[order(pw$ord)])); pw$lab <- sprintf("%.0f%%", pw$VE)
  xlo <- max(-250, floor(min(pw$VE_L)/10)*10)
  try(ggs("F5_protocol_forest.png", ggplot(pw, aes(VE, model, colour = grp)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_errorbarh(aes(xmin = pmax(VE_L, xlo), xmax = pmin(VE_U, 100)), height = 0.22, linewidth = 0.7, na.rm = TRUE) +
    geom_point(size = 3, na.rm = TRUE) + geom_text(aes(label = lab), vjust = -0.9, size = 3.1, show.legend = FALSE, na.rm = TRUE) +
    scale_colour_manual(values = c("Status (reference)" = "#555555", "Protocol window" = "#1B7837", "Sensitivity" = "#2C7FB8"), name = NULL) +
    coord_cartesian(xlim = c(xlo, 100)) +
    labs(title = "Protocol-window vaccine effectiveness (Firth-penalised)", subtitle = "0 cases in every vaccinated-timing subgroup; wide CIs", x = "Vaccine effectiveness (%)", y = NULL) +
    thm + theme(legend.position = "bottom"), w = 8, h = 4.4), silent = TRUE) }

# ---- 10. Save everything ----------------------------------------------------
bundle <- list(
  N = list(distinct=nrow(D), episode=sum(!is.na(D$Y_episode)), analytic=crude$n, cases=crude$cases, controls=crude$controls,
           cross=sum(D$is_cross_record=="Yes"), rep_tested=nrow(rep_tested), vaccinated=nv),
  crude=crude, adj=adj, adj_sap=adj_sap, crude_ex=crude_ex, protocol=protocol, firth_crude=fr_crude, firth_adj=fr_adj,
  model_tbl=model_tbl, sens=sens, char_by_pcr=char_by_pcr, completeness=completeness,
  vacc_timing=vacc_timing, trans_tab=trans_tab, dq=dq,
  extra_models=extra_models, expo_class=expo_class, extra_char=extra_char,
  extra_complete=extra_complete, severity_cases=if (is.null(severity_cases)) data.frame() else severity_cases,
  mortality=mortality_tbl, mortality_counts=mort_counts, mortality_n=nrow(mdat),
  sap_confounders=sap_confounders,
  flow=read.csv(file.path(OUT_DIR,"cohort_flow.csv"), check.names=FALSE))
saveRDS(bundle, file.path(OUT_DIR, "results_bundle.rds"))
write.csv(D[, !names(D) %in% c("onset","vacc_dt","specimen_date")], file.path(OUT_DIR, "analytic_participant_level.csv"), row.names=FALSE, na="")
writexl::write_xlsx(list(Study_flow=bundle$flow, VE_models=model_tbl, Outcome_sensitivity=sens, Completeness=completeness,
  Char_by_PCR=char_by_pcr, Vaccination_timing=vacc_timing, Transitions=trans_tab, DQ_flags=dq,
  Extended_VE_models=extra_models, Exposure_by_casecontrol=expo_class, Extended_characteristics=extra_char,
  Extended_completeness=extra_complete, Severity_among_cases=bundle$severity_cases,
  Mortality_VE=mortality_tbl, SAP_confounders=sap_confounders),
  file.path(OUT_DIR, "analysis_tables.xlsx"))

# console summary (aggregate numbers only — no participant data)
cat("[03] VE models (all):\n")
for (i in seq_len(nrow(model_tbl)))
  cat(sprintf("     - %-52s VE %6s %%  (%-16s)  N=%s  cases/controls=%s/%s\n",
      model_tbl$Model[i], model_tbl$`VE %`[i], model_tbl$`VE 95% CI`[i], model_tbl$N[i], model_tbl$Cases[i], model_tbl$Controls[i]))
cat("[03] outcome-definition sensitivity:\n")
for (i in seq_len(nrow(sens)))
  cat(sprintf("     - %-20s VE %6s %%  (%s)  cases=%s\n", sens$`Outcome definition`[i], sens$`VE %`[i], sens$`VE 95% CI`[i], sens$Cases[i]))
cat("[03] protocol-window exposure (>10d effective vs unvaccinated):\n")
for (r in c("Vaccinated >10d (effective)","Vaccinated 0-10d (not yet protected)","Unvaccinated")) {
  i <- which(expo_class$`Exposure category` == r)
  if (length(i)) cat(sprintf("     %-38s cases=%s  controls=%s\n", r, expo_class$`Case (PCR+)`[i], expo_class$`Control (PCR-)`[i])) }
cat(sprintf("     Firth protocol VE = %s %% (%s)   [ordinary crude separates: 0 vaccinated cases]\n",
    ifelse(is.na(fr_protocol$VE),"NA",sprintf("%.1f",fr_protocol$VE)), ifelse(is.na(fr_protocol$VE_L),"undefined",sprintf("%.1f to %.1f",fr_protocol$VE_L,fr_protocol$VE_U))))
cat("[03] mortality VE (Primary 2 — death among confirmed cases):\n")
cat(sprintf("     %s\n", mort_counts))
for (i in seq_len(nrow(mortality_tbl)))
  cat(sprintf("     - %-32s %-10s (%s)  VE_death %s %% (%s)\n", mortality_tbl$Measure[i], mortality_tbl$Estimate[i], mortality_tbl$`Estimate 95% CI`[i], mortality_tbl$`VE against death %`[i], mortality_tbl$`VE 95% CI`[i]))
cat(sprintf("[03] figures: %s\n", paste(list.files(file.path(OUT_DIR,"figures"), pattern="\\.png$"), collapse=", ")))
cat(sprintf("[03] bundle + tables -> %s\n", OUT_DIR))
