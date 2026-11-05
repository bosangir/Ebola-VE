# =============================================================================
#  Ebola Vaccine Effectiveness (DRC) — PRELIMINARY test-negative analysis
#  ve_analysis.R  —  reproducible analytic engine
#
#  Governing methodology: "Ebola Vaccine Effectiveness in DRC_10Aug2026 Protocol".
#  Design: test-negative (case = suspected + RT-PCR positive; control = suspected
#  + RT-PCR negative). Exposure (Ervebo/rVSV): "effectively vaccinated" if symptom
#  onset > 10 days after vaccination. VE = (1 - OR) x 100%.
#
#  ALL numbers are computed from the supplied data. Nothing is hard-coded.
#  Two linkage datasets are analysed:
#     (A) NARROW  — one definitive lab record per participant (standard build)
#     (B) BROAD   — all lab samples per participant, long format (build_broad)
#
#  Outputs -> EVE_VE_Analysis/{tables,figures,output}
# =============================================================================
suppressWarnings(suppressMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(writexl); library(stringi)
}))
options(stringsAsFactors = FALSE)

ROOT <- "C:/Users/hp/Documents/data/EVE_VE_Analysis"
STD  <- "C:/Users/hp/Documents/data/DashShiny_EVE_BackUp_Shared/data"
BRD  <- "C:/Users/hp/Documents/data/DashShiny_EVE_BackUp_Shared_broad/data"
TBL  <- file.path(ROOT,"tables"); FIG <- file.path(ROOT,"figures"); OUT <- file.path(ROOT,"output")
dir.create(TBL, showWarnings=FALSE, recursive=TRUE)
dir.create(FIG, showWarnings=FALSE, recursive=TRUE)
dir.create(OUT, showWarnings=FALSE, recursive=TRUE)

# ---- helpers ----------------------------------------------------------------
rc     <- function(p) if (file.exists(p)) read.csv(p, colClasses="character", check.names=FALSE, na.strings=character(0), encoding="UTF-8") else NULL
blank  <- function(x){ if (is.null(x)) return(character(0)); x[is.na(x)] <- ""; trimws(x) }
norm   <- function(s){ s<-blank(s); t<-suppressWarnings(iconv(s,to="ASCII//TRANSLIT")); s<-ifelse(is.na(t),s,t); toupper(gsub("\\s+"," ",trimws(s))) }
isRealId <- function(id){ id<-toupper(blank(id)); nzchar(id) & grepl("[0-9]", id) & !grepl("^(RAS|INCONNU|RDC|NA)$", id) }
pkey   <- function(n1,n4,n5) ifelse(isRealId(n1), paste0("ID#",norm(n1)), paste0("NM#",norm(paste0(n4," ",n5))))
pdate  <- function(x){                       # multi-format -> Date
  x <- trimws(blank(x)); out <- as.Date(rep(NA_real_, length(x)), origin="1970-01-01")
  fmts <- c("%Y-%m-%d","%d-%b-%Y","%d/%m/%Y","%d-%m-%Y","%d.%m.%Y","%Y/%m/%d","%d %b %Y")
  for (f in fmts){ miss <- is.na(out) & nzchar(x); if(!any(miss)) break
    out[miss] <- suppressWarnings(as.Date(x[miss], format=f)) }
  # Excel serials
  ser <- is.na(out) & grepl("^[0-9]{5}$", x); if(any(ser)) out[ser] <- as.Date(as.numeric(x[ser]), origin="1899-12-30")
  out }
numv   <- function(x){ suppressWarnings(as.numeric(gsub("[^0-9.\\-]","", blank(x)))) }
pct    <- function(n,d) ifelse(d>0, round(100*n/d,1), NA)
np     <- function(n,d) sprintf("%d (%.1f%%)", n, ifelse(d>0,100*n/d,0))
`%||%` <- function(a,b) if (is.null(a)||length(a)==0) b else a

# code maps
MAP_VACC <- c("1"="Vaccinated","2"="Not vaccinated","99"="Unknown")
MAP_SEX  <- c("1"="Male","2"="Female","3"="Unknown")
MAP_PCRS <- c("1"="Positive","2"="Negative","3"="Indeterminate","4"="Not available")
MAP_TYPE <- c("1"="rVSV-ZEBOV (Ervebo, assumed)","99"="Unknown type","2"="Other")

# =============================================================================
# 1. IMPORT
# =============================================================================
sv   <- rc(file.path(STD,"raw_data","ebola_vaccine_effectiveness_surveillance.csv"))
lk   <- rc(file.path(STD,"linked.csv"))                 # narrow linkage map (surv <-> one lab patient)
lg   <- rc(file.path(BRD,"linked_long.csv"))            # broad: all lab samples per linked patient
merg <- rc(file.path(STD,"raw_data","merged_eve_lab_data.csv"))
stopifnot(!is.null(sv), !is.null(lk), !is.null(lg))

N_sv_raw  <- nrow(sv); N_lab_raw <- nrow(merg); N_samp_raw <- nrow(lg)

# =============================================================================
# 2. STANDARDIZE + CLEAN surveillance (the participant/exposure/clinical source)
# =============================================================================
S <- tibble(
  key0        = pkey(sv$name_1, sv$name_4, sv$name_5),
  case_id     = blank(sv$name_1),
  surname     = blank(sv$name_4), firstname = blank(sv$name_5),
  sex         = MAP_SEX[blank(sv$sex)] %||% "",
  age_years   = numv(sv$age_years),
  health_zone = stri_trans_totitle(blank(sv$health_zone_1)),
  health_area = stri_trans_totitle(blank(sv$health_area_1)),
  village     = stri_trans_totitle(blank(sv$village_residence)),
  onset_date  = pdate(sv$date_frist_sympt),
  specimen_date_surv = pdate(sv$specimen_collection_date),
  pcr_surv    = MAP_PCRS[blank(sv$pcr_result)] %||% "",
  vacc_status = MAP_VACC[blank(sv$vaccin_mve)] %||% "",
  vacc_date   = pdate(sv$date_vaccination1),
  vacc_type   = MAP_TYPE[blank(sv$type_vaccin1)] %||% "",
  vacc_strategy_code = blank(sv$vaccine_strategy),
  vacc_source = blank(sv$source_vacccination)
)
S$sex[is.na(S$sex)] <- ""; S$vacc_status[is.na(S$vacc_status)|S$vacc_status==""] <- "Unknown"
S$vacc_type[is.na(S$vacc_type)] <- ""
# vaccination strategy is coded 1-5 in the source with NO decode key available ->
# keep the raw code, flag that ring vs targeted-geographic cannot be resolved yet.
S$vacc_strategy <- ifelse(nzchar(S$vacc_strategy_code), paste0("Strategy code ", S$vacc_strategy_code), "Unknown/not recorded")

# =============================================================================
# 3. DEDUPLICATION (surveillance participant records) + AUDIT
#    Source-priority rules (documented):
#      * RT-PCR result & specimen date  -> LABORATORY source (preferred)
#      * vaccination status/date/type/strategy -> vaccination register; NOT
#        available as a separate file, so surveillance vaccination fields are
#        used (documented limitation).
#      * symptom-onset date -> surveillance; earliest clinically plausible kept.
#    Conflicting observations are NOT deleted; discrepancy flags are preserved.
# =============================================================================
# exact/probable duplicate surveillance participants share the identity key (full
# name, or real case id). Keep ONE record: the most complete (has onset, then has
# age), earliest onset as tie-break.
S <- S %>% mutate(.complete = (!is.na(onset_date)) + (!is.na(age_years)) + nzchar(pcr_surv))
S_ord <- S %>% arrange(key0, desc(.complete), onset_date)
dup_flag <- duplicated(S_ord$key0)
S_dedup  <- S_ord[!dup_flag, ] %>% select(-.complete)
n_surv_dupgroups <- sum(duplicated(S_ord$key0))                 # extra rows removed
n_surv_unique    <- nrow(S_dedup)

# =============================================================================
# 4. LINKAGE MAP + TIER  (narrow)  and  LAB HISTORIES (broad, long)
# =============================================================================
# narrow linkage: surveillance key -> lab patient_uid + tier + single lab result
lk <- lk %>% mutate(key0 = pkey(name_1, name_4, name_5),
                    lab_result_single = blank(`FINAL RESULT (EVD)2`),
                    tier = blank(tier),
                    patient_uid = blank(final_blp))
# collapse to best tier per participant (Tier 1 < 2 < 3 < 4 ; keep lowest tier number)
lk$tier_n <- suppressWarnings(as.integer(sub("Tier\\s*([0-9]).*","\\1", lk$tier)))
lk_best <- lk %>% arrange(key0, tier_n) %>% filter(!duplicated(key0))

link_method <- function(tn) dplyr::case_when(
  tn==1 ~ "Automatic / exact (Tier 1)",
  tn==2 ~ "Probabilistic (Tier 2)",
  tn %in% c(3,4) ~ "Manual / adjudicated (Tier 3-4)",
  TRUE ~ "Unlinked")

# broad lab histories: all samples per participant key
lg <- lg %>% mutate(key0 = pkey(surv_case_id, surv_surname, surv_firstname),
                    result = blank(result),
                    samp_date = pdate(sampling_date),
                    ct_gp = numv(genexpert_ct_gp),
                    seq = suppressWarnings(as.integer(sample_seq)))
lg_def <- lg %>% filter(result %in% c("Positive","Negative"))

hist_of <- function(k, onset){
  g <- lg_def %>% filter(key0==k) %>% arrange(samp_date, seq)
  if (nrow(g)==0) return(NULL)
  res <- g$result; dt <- g$samp_date
  ispos <- res=="Positive"
  # episode-defining: first definitive specimen on/after onset; else first definitive
  epi_i <- if (!is.na(onset)) which(dt >= onset)[1] else NA
  if (is.na(epi_i)) epi_i <- 1L
  seqchar <- paste(substr(res,1,1), collapse="")
  # transition class from first vs last definitive
  fr <- res[1]; lr <- res[length(res)]
  changed <- length(unique(res))>1
  trans <- if (length(res)<2) paste0("Single (",substr(fr,1,1),")")
           else if (fr=="Negative" & lr=="Positive") "N->P"
           else if (fr=="Positive" & lr=="Negative") "P->N"
           else if (fr=="Negative" & lr=="Negative") { if (changed) "other" else "N->N" }
           else if (fr=="Positive" & lr=="Positive") { if (changed) "other" else "P->P" }
           else "other"
  list(n_tests=nrow(g), n_def=nrow(g),
       first_specimen=dt[1], last_specimen=dt[length(dt)],
       first_result=fr, final_result=lr,
       episode_result=res[epi_i], episode_specimen=dt[epi_i],
       any_pos=any(ispos),
       date_first_pos=if(any(ispos)) dt[which(ispos)[1]] else as.Date(NA),
       date_last_pos =if(any(ispos)) dt[rev(which(ispos))[1]] else as.Date(NA),
       trajectory=seqchar, changed=changed, transition=trans)
}

# =============================================================================
# 5. BUILD PARTICIPANT-LEVEL ANALYTIC DATASET
# =============================================================================
P <- S_dedup
mi <- match(P$key0, lk_best$key0)
P$surv_name     <- trimws(paste(P$surname, P$firstname))
P$linked_tier_n <- lk_best$tier_n[mi]
P$patient_uid   <- lk_best$patient_uid[mi]
P$lab_single    <- lk_best$lab_result_single[mi]
# ---- lab-side identifiers carried through for manual verification of the
#      linkage (esp. the less-trusted Tier 3-4 / manual-adjudicated links) ----
P$lab_name      <- lk_best$NAMES[mi]
P$lab_age       <- numv(lk_best$AGE[mi])
P$surv_age      <- P$age_years
P$age_diff_years<- ifelse(!is.na(P$surv_age) & !is.na(P$lab_age), abs(P$surv_age - P$lab_age), NA_real_)
P$sex_agreement <- lk_best$sex_agreement[mi]
P$linked        <- !is.na(P$linked_tier_n)
P$link_method   <- link_method(P$linked_tier_n)
P$link_method[!P$linked] <- "Unlinked"

# lab outcome definitions per participant
outcols <- c("n_tests","first_specimen","last_specimen","first_result","final_result",
             "episode_result","episode_specimen","any_pos","date_first_pos","date_last_pos",
             "trajectory","changed","transition")
for (c in outcols) P[[c]] <- NA
P$first_specimen<-as.Date(P$first_specimen); P$last_specimen<-as.Date(P$last_specimen)
P$episode_specimen<-as.Date(P$episode_specimen); P$date_first_pos<-as.Date(P$date_first_pos); P$date_last_pos<-as.Date(P$date_last_pos)

for (i in which(P$linked)){
  h <- hist_of(P$key0[i], P$onset_date[i])
  if (!is.null(h)){
    for (c in outcols) P[[c]][i] <- if (inherits(P[[c]], "Date")) as.Date(h[[c]]) else h[[c]]
  } else {
    # linked but no multi-sample history -> use single merged lab result
    r <- P$lab_single[i]
    if (r %in% c("Positive","Negative")){
      P$n_tests[i]<-1; P$first_result[i]<-r; P$final_result[i]<-r; P$episode_result[i]<-r
      P$any_pos[i]<- r=="Positive"; P$trajectory[i]<-substr(r,1,1); P$changed[i]<-FALSE
      P$transition[i]<-paste0("Single (",substr(r,1,1),")")
    }
  }
}
# where still no lab history but single result exists (safety)
need <- P$linked & is.na(P$episode_result) & P$lab_single %in% c("Positive","Negative")
P$episode_result[need]<-P$lab_single[need]; P$first_result[need]<-P$lab_single[need]
P$final_result[need]<-P$lab_single[need]; P$any_pos[need]<-P$lab_single[need]=="Positive"
P$n_tests[need]<-ifelse(is.na(P$n_tests[need]),1,P$n_tests[need])

# outcome Y under four definitions (1=positive case, 0=negative control, NA=no definitive result)
yfun <- function(res) ifelse(res=="Positive",1L, ifelse(res=="Negative",0L, NA_integer_))
P$Y_episode <- yfun(P$episode_result)
P$Y_first   <- yfun(P$first_result)
P$Y_final   <- yfun(P$final_result)
P$Y_any     <- ifelse(is.na(P$any_pos), NA_integer_, ifelse(P$any_pos,1L,
                 ifelse(P$episode_result=="Negative"|P$first_result=="Negative"|P$final_result=="Negative",0L,NA_integer_)))

# =============================================================================
# 6. EXPOSURE (protocol) + INTERVALS
# =============================================================================
P$specimen_date <- as.Date(ifelse(!is.na(P$episode_specimen), P$episode_specimen,
                           ifelse(!is.na(P$first_specimen), P$first_specimen, P$specimen_date_surv)), origin="1970-01-01")
P$onset_to_swab <- as.numeric(P$specimen_date - P$onset_date)
P$vax_to_onset  <- as.numeric(P$onset_date - P$vacc_date)

# exposure categories (Ervebo/rVSV window = 10 days)
WIN <- 10
P$exposure <- dplyr::case_when(
  P$vacc_status=="Not vaccinated" ~ "Unvaccinated",
  P$vacc_status=="Vaccinated" & !is.na(P$vax_to_onset) & P$vax_to_onset >  WIN ~ "Vaccinated >10d (effective)",
  P$vacc_status=="Vaccinated" & !is.na(P$vax_to_onset) & P$vax_to_onset >= 0 & P$vax_to_onset <= WIN ~ "Vaccinated 0-10d (not yet protected)",
  P$vacc_status=="Vaccinated" & !is.na(P$vax_to_onset) & P$vax_to_onset <  0 ~ "Vaccinated after onset (ineligible)",
  P$vacc_status=="Vaccinated" & is.na(P$vax_to_onset) ~ "Vaccinated, timing unknown",
  TRUE ~ "Unknown status")
# binary exposure for the primary model (status-based): vaccinated vs unvaccinated
P$V_status <- dplyr::case_when(P$vacc_status=="Vaccinated" ~ 1L,
                               P$vacc_status=="Not vaccinated" ~ 0L, TRUE ~ NA_integer_)
# protocol window exposure (effective vs unvaccinated); others -> NA (separate)
P$V_effective <- dplyr::case_when(P$exposure=="Unvaccinated" ~ 0L,
                                  P$exposure=="Vaccinated >10d (effective)" ~ 1L, TRUE ~ NA_integer_)

# covariates
P$agegrp     <- cut(P$age_years, breaks=c(-Inf,5,17,44,Inf), labels=c("0-5","6-17","18-44","45+"))
P$sex_f      <- factor(ifelse(P$sex %in% c("Male","Female"), P$sex, NA), levels=c("Female","Male"))
P$zone_f     <- factor(ifelse(P$health_zone=="Bulape","Bulape","Other/Unknown"))
P$onset_month<- factor(format(P$onset_date, "%Y-%m"))
P$caltime    <- as.numeric(P$onset_date - min(P$onset_date, na.rm=TRUE))/7   # weeks since first onset

saveRDS(P, file.path(OUT,"analytic_participant_level.rds"))
write.csv(P, file.path(OUT,"analytic_participant_level.csv"), row.names=FALSE, na="")

# =============================================================================
# 7. AUDIT / DATA-FLOW TABLE
# =============================================================================
orph_pos <- { pr <- blank(merg$pcr_test_result); uid <- blank(merg$patient_uid)
              linked_uid <- unique(blank(lk$patient_uid))
              sum(pr=="Positive" & !(uid %in% linked_uid)) }
audit <- tibble(Step = c(
  "Surveillance case-investigation records imported",
  "Merged laboratory patient records imported",
  "Laboratory sample-level observations imported (long)",
  "Duplicate surveillance participant records identified (shared identity key)",
  "Duplicate surveillance records removed / consolidated",
  "Unique surveillance participants remaining",
  "Unique participants successfully linked to laboratory data",
  "  of which Tier 1 automatic/exact",
  "  of which Tier 2 probabilistic",
  "  of which Tier 3-4 manual/adjudicated",
  "Linked participants with a definitive RT-PCR result (case/control eligible)",
  "Unresolved: linked but no definitive RT-PCR result",
  "Unlinked surveillance participants",
  "PCR-positive laboratory records without a surveillance match (orphan positives)"),
  N = c(N_sv_raw, N_lab_raw, N_samp_raw,
        n_surv_dupgroups, n_surv_dupgroups, n_surv_unique,
        sum(P$linked),
        sum(P$linked_tier_n==1, na.rm=TRUE), sum(P$linked_tier_n==2, na.rm=TRUE),
        sum(P$linked_tier_n %in% c(3,4), na.rm=TRUE),
        sum(P$linked & !is.na(P$Y_episode)),
        sum(P$linked & is.na(P$Y_episode)),
        sum(!P$linked), orph_pos))
write.csv(audit, file.path(TBL,"T1_data_flow_audit.csv"), row.names=FALSE)

# ---- LINKAGE VERIFICATION table: surveillance vs lab identity side-by-side.
#      Sorted to surface the least-trusted links first (Tier 3-4 manual, then
#      largest age gap / sex disagreement) so they can be eyeballed & confirmed. ----
verify <- P %>% filter(linked) %>%
  transmute(link_method, tier=linked_tier_n, patient_uid,
            surv_name, surv_age, surv_sex=sex,
            lab_name, lab_age, age_diff_years, sex_agreement,
            episode_result, n_tests) %>%
  arrange(desc(tier), desc(is.na(age_diff_years)), desc(age_diff_years))
write.csv(verify, file.path(TBL,"T11_linkage_verification.csv"), row.names=FALSE)
verify_manual <- verify %>% filter(tier %in% c(3,4))
write.csv(verify_manual, file.path(TBL,"T11b_linkage_verification_manual_T3T4.csv"), row.names=FALSE)

# =============================================================================
# 8. COMPLETENESS (Pierre) — for NARROW (linked, one result/participant) and
#    BROAD (participants with >=1 lab sample). Reported as n complete (%) .
# =============================================================================
completeness <- function(df, label){
  n <- nrow(df)
  has <- function(v) sum(!is.na(v) & (if(is.character(v)) nzchar(v) else TRUE))
  vac_dated <- df$vacc_status=="Vaccinated"
  tibble(Variable=c("RT-PCR result (definitive)","Symptom-onset date","Vaccination status",
    "Vaccination date (among vaccinated)","Age","Sex","Health zone","Health area","Village",
    "Vaccination strategy","Specimen/swab date","Linkage method/tier"),
    Dataset=label, N=n,
    Complete=c(
      sum(!is.na(df$Y_episode)),
      sum(!is.na(df$onset_date)),
      sum(df$vacc_status %in% c("Vaccinated","Not vaccinated")),
      sum(vac_dated & !is.na(df$vacc_date)),
      sum(!is.na(df$age_years)),
      sum(df$sex %in% c("Male","Female")),
      sum(nzchar(df$health_zone)),
      sum(nzchar(df$health_area)),
      sum(nzchar(df$village)),
      sum(df$vacc_strategy!="Unknown/not recorded"),
      sum(!is.na(df$specimen_date)),
      sum(df$linked)),
    Denominator=c(n,n,n, sum(vac_dated), n,n,n,n,n, n, n, n)) %>%
    mutate(`Complete n (%)`=sprintf("%d (%.1f%%)",Complete,ifelse(Denominator>0,100*Complete/Denominator,0)),
           `Missing n (%)` =sprintf("%d (%.1f%%)",Denominator-Complete,ifelse(Denominator>0,100*(Denominator-Complete)/Denominator,0)))
}
P_narrow <- P %>% filter(linked)
P_broad  <- P %>% filter(linked & !is.na(n_tests))          # participants with lab history/sample(s)
cmp_narrow <- completeness(P_narrow,"Narrow (1 result/participant)")
cmp_broad  <- completeness(P_broad ,"Broad (all samples)")
cmp_all <- bind_rows(cmp_narrow, cmp_broad)
write.csv(cmp_all, file.path(TBL,"T2_completeness_Pierre.csv"), row.names=FALSE)

# side-by-side comparison table (narrow vs broad)
cmp_cmp <- cmp_narrow %>% select(Variable, narrow=`Complete n (%)`) %>%
  left_join(cmp_broad %>% select(Variable, broad=`Complete n (%)`), by="Variable")
write.csv(cmp_cmp, file.path(TBL,"T2b_completeness_narrow_vs_broad.csv"), row.names=FALSE)

# =============================================================================
# 9. DESCRIPTIVE DISTRIBUTIONS
# =============================================================================
freqtab <- function(x, lvls=NULL){ x<-ifelse(is.na(x)|x=="","Missing",as.character(x))
  if(is.null(lvls)) lvls<-names(sort(table(x),decreasing=TRUE));
  tibble(Category=lvls, n=as.integer(table(factor(x,levels=lvls)))) %>% mutate(`%`=round(100*n/sum(n),1)) }
dist_pcr   <- freqtab(P_narrow$episode_result)
dist_vacc  <- freqtab(P_narrow$vacc_status, c("Vaccinated","Not vaccinated","Unknown"))
dist_strat <- freqtab(P_narrow$vacc_strategy)
dist_sex   <- freqtab(P_narrow$sex)
dist_zone  <- freqtab(P_narrow$health_zone)
dist_tier  <- freqtab(P_narrow$link_method, c("Automatic / exact (Tier 1)","Probabilistic (Tier 2)","Manual / adjudicated (Tier 3-4)"))
dist_expo  <- freqtab(P_narrow$exposure)
age_summary<- P_narrow %>% summarise(n=sum(!is.na(age_years)), median=median(age_years,na.rm=TRUE),
                q1=quantile(age_years,.25,na.rm=TRUE), q3=quantile(age_years,.75,na.rm=TRUE),
                mean=round(mean(age_years,na.rm=TRUE),1), sd=round(sd(age_years,na.rm=TRUE),1))
write_xlsx(list(PCR=dist_pcr, Vaccination=dist_vacc, Strategy=dist_strat, Sex=dist_sex,
  HealthZone=dist_zone, LinkageMethod=dist_tier, ExposureWindow=dist_expo, Age=age_summary),
  file.path(TBL,"T3_descriptive_distributions.xlsx"))

# characteristics by PCR status (episode outcome)
byS <- P_narrow %>% filter(!is.na(Y_episode)) %>% mutate(grp=ifelse(Y_episode==1,"Case (PCR+)","Control (PCR-)"))
char_by_pcr <- bind_rows(
  byS %>% group_by(grp) %>% summarise(Variable="N", value=as.character(n()), .groups="drop"),
  byS %>% group_by(grp) %>% summarise(Variable="Age median (IQR)",
     value=sprintf("%.0f (%.0f-%.0f)", median(age_years,na.rm=TRUE), quantile(age_years,.25,na.rm=TRUE), quantile(age_years,.75,na.rm=TRUE)), .groups="drop"),
  byS %>% group_by(grp) %>% summarise(Variable="Female n (%)",
     value=np(sum(sex=="Female"), n()), .groups="drop"),
  byS %>% group_by(grp) %>% summarise(Variable="Vaccinated n (%)",
     value=np(sum(vacc_status=="Vaccinated"), n()), .groups="drop"),
  byS %>% group_by(grp) %>% summarise(Variable="Health zone Bulape n (%)",
     value=np(sum(health_zone=="Bulape"), n()), .groups="drop")) %>%
  pivot_wider(names_from=grp, values_from=value)
write.csv(char_by_pcr, file.path(TBL,"T4_characteristics_by_pcr.csv"), row.names=FALSE)

# vaccination by case/control
vc_tab <- byS %>% filter(vacc_status %in% c("Vaccinated","Not vaccinated")) %>%
  count(grp, vacc_status) %>% pivot_wider(names_from=vacc_status, values_from=n, values_fill=0)
write.csv(vc_tab, file.path(TBL,"T5_vaccination_by_casecontrol.csv"), row.names=FALSE)

# =============================================================================
# 10. RT-PCR TRANSITIONS (repeatedly tested participants)  — Analysis 2
# =============================================================================
rep_tested <- P_narrow %>% filter(!is.na(n_tests) & n_tests>=2)
trans_tab  <- freqtab(rep_tested$transition,
   c("N->N","N->P","P->P","P->N","other"))
write.csv(trans_tab, file.path(TBL,"T6_pcr_transition.csv"), row.names=FALSE)

# =============================================================================
# 11. MODELS — crude & adjusted logistic; VE = (1-OR)*100 with CI from OR CI
# =============================================================================
# compact Firth logistic (Jeffreys-prior penalized) for sparse-data sensitivity
firth <- function(X, y, maxit=200, tol=1e-9){
  b <- rep(0, ncol(X))
  for (it in 1:maxit){
    p <- as.vector(plogis(X %*% b)); W <- p*(1-p); Wr <- sqrt(W)
    XtWX <- crossprod(X*Wr); XtWXinv <- solve(XtWX)
    Q <- (X*Wr) %*% XtWXinv; h <- rowSums(Q * (X*Wr))          # hat diagonal
    U <- crossprod(X, (y - p) + h*(0.5 - p))
    step <- XtWXinv %*% U; b <- b + step
    if (max(abs(step)) < tol) break
  }
  se <- sqrt(diag(XtWXinv)); list(coef=as.vector(b), se=se)
}
ve_from_or <- function(or, lo, hi){ or<-unname(or); lo<-unname(lo); hi<-unname(hi)
  c(VE=(1-or)*100, VE_L=(1-hi)*100, VE_U=(1-lo)*100) }

fit_logit <- function(dat, outcome, exposure="V_status", adj=NULL){
  dat <- dat[!is.na(dat[[outcome]]) & !is.na(dat[[exposure]]), , drop=FALSE]
  form_terms <- c(exposure, adj)
  # keep only covariates with >1 level and acceptable completeness
  keep_adj <- character(0)
  for (a in adj){ v<-dat[[a]]; if (length(unique(v[!is.na(v)]))>1) keep_adj<-c(keep_adj,a) }
  if (!is.null(adj)) dat <- dat[complete.cases(dat[, c(outcome,exposure,keep_adj)]), , drop=FALSE]
  f <- as.formula(paste(outcome, "~", paste(c(exposure, keep_adj), collapse=" + ")))
  m <- tryCatch(glm(f, data=dat, family=binomial()), error=function(e) NULL)
  n <- nrow(dat); cases <- sum(dat[[outcome]]==1); ctrls <- sum(dat[[outcome]]==0)
  if (is.null(m)) return(list(ok=FALSE, n=n, cases=cases, controls=ctrls))
  ci <- suppressWarnings(tryCatch(confint(m), error=function(e) confint.default(m)))
  co <- coef(m); enm <- grep(paste0("^",exposure), names(co), value=TRUE)[1]
  or <- exp(co[enm]); lo <- exp(ci[enm,1]); hi <- exp(ci[enm,2])
  ve <- ve_from_or(or, lo, hi)
  # separation check
  sep <- any(abs(co) > 15) || any(!is.finite(ci[enm,]))
  list(ok=TRUE, model=m, n=n, cases=cases, controls=ctrls, adj=keep_adj,
       OR=or, OR_L=lo, OR_U=hi, VE=ve["VE"], VE_L=ve["VE_L"], VE_U=ve["VE_U"], sep=sep,
       exposure=exposure, outcome=outcome)
}
firth_or <- function(dat, outcome, exposure="V_status", adj=NULL){
  keep_adj <- adj[sapply(adj, function(a) length(unique(dat[[a]][!is.na(dat[[a]])]))>1)] %||% NULL
  vars <- c(outcome, exposure, keep_adj)
  dat <- dat[complete.cases(dat[,vars,drop=FALSE]), , drop=FALSE]
  mm <- model.matrix(as.formula(paste("~", paste(c(exposure,keep_adj),collapse="+"))), data=dat)
  y <- dat[[outcome]]; fit <- firth(mm, y)
  enm <- grep(paste0("^",exposure), colnames(mm), value=TRUE)[1]; j <- which(colnames(mm)==enm)
  b <- fit$coef[j]; se <- fit$se[j]; or<-exp(b); lo<-exp(b-1.96*se); hi<-exp(b+1.96*se)
  ve <- ve_from_or(or,lo,hi)
  list(OR=or,OR_L=lo,OR_U=hi,VE=ve["VE"],VE_L=ve["VE_L"],VE_U=ve["VE_U"],n=nrow(dat),
       cases=sum(y==1),controls=sum(y==0))
}

ADJ <- c("agegrp","sex_f","caltime")     # parsimonious: age, sex, calendar time (geography ~invariant: 95% Bulape)
crude_status <- fit_logit(P_narrow, "Y_episode", "V_status", adj=NULL)
adj_status   <- fit_logit(P_narrow, "Y_episode", "V_status", adj=ADJ)
crude_eff    <- fit_logit(P_narrow, "Y_episode", "V_effective", adj=NULL)   # protocol >10d window (preliminary)
firth_status <- firth_or(P_narrow, "Y_episode", "V_status", adj=NULL)

model_tbl <- tibble(
  Model=c("Crude (vaccinated vs unvaccinated)","Adjusted (age, sex, calendar time)",
          "Crude, protocol >10d effective window (preliminary)","Firth crude (sparse-data sensitivity)"),
  N=c(crude_status$n, adj_status$n, crude_eff$n, firth_status$n),
  Cases=c(crude_status$cases, adj_status$cases, crude_eff$cases, firth_status$cases),
  Controls=c(crude_status$controls, adj_status$controls, crude_eff$controls, firth_status$controls),
  OR=sprintf("%.2f", c(crude_status$OR, adj_status$OR, crude_eff$OR, firth_status$OR)),
  `OR 95% CI`=c(sprintf("%.2f-%.2f",crude_status$OR_L,crude_status$OR_U),
                sprintf("%.2f-%.2f",adj_status$OR_L,adj_status$OR_U),
                sprintf("%.2f-%.2f",crude_eff$OR_L,crude_eff$OR_U),
                sprintf("%.2f-%.2f",firth_status$OR_L,firth_status$OR_U)),
  `VE %`=sprintf("%.1f", c(crude_status$VE, adj_status$VE, crude_eff$VE, firth_status$VE)),
  `VE 95% CI`=c(sprintf("%.1f to %.1f",crude_status$VE_L,crude_status$VE_U),
                sprintf("%.1f to %.1f",adj_status$VE_L,adj_status$VE_U),
                sprintf("%.1f to %.1f",crude_eff$VE_L,crude_eff$VE_U),
                sprintf("%.1f to %.1f",firth_status$VE_L,firth_status$VE_U)))
write.csv(model_tbl, file.path(TBL,"T8_VE_models.csv"), row.names=FALSE)

# =============================================================================
# 12. SENSITIVITY across outcome definitions (Analysis 2)
# =============================================================================
outdefs <- c(`Episode-defining (primary)`="Y_episode", `First result`="Y_first",
             `Final result`="Y_final", `Any-positive`="Y_any")
sens <- lapply(names(outdefs), function(nm){
  oc <- outdefs[[nm]]; m <- fit_logit(P_narrow, oc, "V_status", adj=NULL)
  d <- P_narrow[!is.na(P_narrow[[oc]]) & !is.na(P_narrow$V_status),]
  tibble(`Outcome definition`=nm, `Total N`=m$n, Cases=m$cases, Controls=m$controls,
    `Vaccinated cases`=sum(d[[oc]]==1 & d$V_status==1), `Vaccinated controls`=sum(d[[oc]]==0 & d$V_status==1),
    OR=sprintf("%.2f",m$OR), `95% CI`=sprintf("%.2f-%.2f",m$OR_L,m$OR_U),
    `VE %`=sprintf("%.1f",m$VE), `VE 95% CI`=sprintf("%.1f to %.1f",m$VE_L,m$VE_U))
}) %>% bind_rows()
write.csv(sens, file.path(TBL,"T7_outcome_sensitivity.csv"), row.names=FALSE)

# =============================================================================
# 13. DATA-QUALITY / TIMING
# =============================================================================
dq <- tibble(Check=c(
  "Vaccination after symptom onset (implausible for protection)",
  "Specimen collected before symptom onset",
  "Onset-to-swab interval > 30 days",
  "Vaccinated but vaccination date missing",
  "Vaccinated but vaccine type unknown",
  "Linked but RT-PCR result indeterminate/missing"),
  N=c(sum(P_narrow$vax_to_onset<0, na.rm=TRUE),
      sum(P_narrow$onset_to_swab<0, na.rm=TRUE),
      sum(P_narrow$onset_to_swab>30, na.rm=TRUE),
      sum(P_narrow$vacc_status=="Vaccinated" & is.na(P_narrow$vacc_date)),
      sum(P_narrow$vacc_status=="Vaccinated" & P_narrow$vacc_type=="Unknown type"),
      sum(P_narrow$linked & is.na(P_narrow$Y_episode))))
write.csv(dq, file.path(TBL,"T9_data_quality_timing.csv"), row.names=FALSE)
interval_summ <- tibble(
  Interval=c("Onset-to-swab (days)","Vaccination-to-onset (days)"),
  n=c(sum(!is.na(P_narrow$onset_to_swab)), sum(!is.na(P_narrow$vax_to_onset))),
  median=c(median(P_narrow$onset_to_swab,na.rm=TRUE), median(P_narrow$vax_to_onset,na.rm=TRUE)),
  IQR=c(sprintf("%.0f to %.0f", quantile(P_narrow$onset_to_swab,.25,na.rm=TRUE), quantile(P_narrow$onset_to_swab,.75,na.rm=TRUE)),
        sprintf("%.0f to %.0f", quantile(P_narrow$vax_to_onset,.25,na.rm=TRUE), quantile(P_narrow$vax_to_onset,.75,na.rm=TRUE))))
write.csv(interval_summ, file.path(TBL,"T10_intervals.csv"), row.names=FALSE)

# =============================================================================
# 14. FIGURES
# =============================================================================
thm <- theme_minimal(base_size=11) + theme(panel.grid.minor=element_blank(),
        plot.title=element_text(face="bold"))
gg <- function(f,p,w=7,h=4.2){ ggsave(file.path(FIG,f), p, width=w, height=h, dpi=150, bg="white") }
try({ gg("F1_vacc_to_onset.png",
  ggplot(P_narrow %>% filter(!is.na(vax_to_onset)), aes(vax_to_onset)) +
   geom_histogram(binwidth=5, fill="#2C7FB8", colour="white") +
   geom_vline(xintercept=10, linetype="dashed", colour="#C62828") +
   labs(title="Vaccination-to-onset interval", subtitle="dashed line = 10-day Ervebo protection window",
        x="Days from vaccination to symptom onset", y="Participants") + thm) }, silent=TRUE)
try({ gg("F2_onset_to_swab.png",
  ggplot(P_narrow %>% filter(!is.na(onset_to_swab), onset_to_swab>=-10, onset_to_swab<=60), aes(onset_to_swab)) +
   geom_histogram(binwidth=3, fill="#31A354", colour="white") +
   labs(title="Onset-to-specimen interval", x="Days from symptom onset to specimen collection", y="Participants") + thm) }, silent=TRUE)
try({ gg("F3_transitions.png",
  ggplot(trans_tab %>% filter(n>0), aes(reorder(Category,n), n)) + geom_col(fill="#756BB1") +
   geom_text(aes(label=n), hjust=-0.2, size=3.5) + coord_flip() +
   labs(title="RT-PCR result transition among repeatedly tested participants", x=NULL, y="Participants") + thm) }, silent=TRUE)
try({ gg("F4_age_by_pcr.png",
  ggplot(byS %>% filter(!is.na(age_years)), aes(grp, age_years, fill=grp)) + geom_boxplot(alpha=.8) +
   scale_fill_manual(values=c("Case (PCR+)"="#C62828","Control (PCR-)"="#2C7FB8"), guide="none") +
   labs(title="Age by RT-PCR status", x=NULL, y="Age (years)") + thm) }, silent=TRUE)
# missingness plot (narrow)
miss_df <- cmp_narrow %>% mutate(Missing_pct=100-100*Complete/Denominator)
try({ gg("F5_missingness.png",
  ggplot(miss_df, aes(reorder(Variable,Missing_pct), Missing_pct)) + geom_col(fill="#E6550D") +
   geom_text(aes(label=sprintf("%.0f%%",Missing_pct)), hjust=-0.15, size=3) + coord_flip() +
   labs(title="Missingness by variable (narrow linked dataset)", x=NULL, y="% missing") +
   ylim(0,100) + thm) }, silent=TRUE)
# forest plot of OR across outcome definitions
fp <- sens %>% mutate(OR=as.numeric(OR),
   L=as.numeric(sub("-.*","",`95% CI`)), U=as.numeric(sub(".*-","",`95% CI`)),
   def=factor(`Outcome definition`, levels=rev(`Outcome definition`)))
try({ gg("F6_forest_OR.png",
  ggplot(fp, aes(OR, def)) + geom_point(size=2.6, colour="#2C3E82") +
   geom_errorbarh(aes(xmin=L, xmax=U), height=.18, colour="#2C3E82") +
   geom_vline(xintercept=1, linetype="dashed", colour="grey40") +
   scale_x_log10() + labs(title="Odds ratio for vaccination by outcome definition",
   subtitle="OR<1 favours vaccine protection (test-negative design)", x="Odds ratio (log scale)", y=NULL) + thm) }, silent=TRUE)

# =============================================================================
# 15. EXPORT combined Excel + results bundle for the report
# =============================================================================
write_xlsx(list(
  DataFlow_Audit=audit, Completeness=cmp_all, Completeness_Compare=cmp_cmp,
  Char_by_PCR=char_by_pcr, Vacc_by_CaseControl=vc_tab, Transitions=trans_tab,
  OutcomeSensitivity=sens, VE_Models=model_tbl, DataQuality=dq, Intervals=interval_summ,
  LinkageVerification=verify, LinkVerify_ManualT3T4=verify_manual),
  file.path(TBL,"EVE_all_tables.xlsx"))

results <- list(
  N=list(sv_raw=N_sv_raw, lab_raw=N_lab_raw, samp_raw=N_samp_raw,
         surv_unique=n_surv_unique, dup=n_surv_dupgroups,
         linked=sum(P$linked), analytic=crude_status$n,
         cases=crude_status$cases, controls=crude_status$controls,
         rep_tested=nrow(rep_tested), discordant=sum(rep_tested$transition %in% c("N->P","P->N","other")),
         orph_pos=orph_pos),
  audit=audit, cmp_narrow=cmp_narrow, cmp_broad=cmp_broad, cmp_cmp=cmp_cmp,
  dist=list(pcr=dist_pcr,vacc=dist_vacc,strat=dist_strat,sex=dist_sex,zone=dist_zone,tier=dist_tier,expo=dist_expo,age=age_summary),
  char_by_pcr=char_by_pcr, vc_tab=vc_tab, trans_tab=trans_tab,
  model_tbl=model_tbl, sens=sens, dq=dq, interval_summ=interval_summ,
  crude=crude_status, adj=adj_status, eff=crude_eff, firth=firth_status)
saveRDS(results, file.path(OUT,"results_bundle.rds"))

cat("\n================ VE ANALYSIS COMPLETE ================\n")
cat(sprintf("Surveillance raw: %d | unique participants: %d | linked: %d\n", N_sv_raw, n_surv_unique, sum(P$linked)))
cat(sprintf("Primary analytic N: %d  (cases %d, controls %d)\n", crude_status$n, crude_status$cases, crude_status$controls))
cat(sprintf("Crude  OR %.2f (%.2f-%.2f)  VE %.1f%% (%.1f to %.1f)\n", crude_status$OR,crude_status$OR_L,crude_status$OR_U,crude_status$VE,crude_status$VE_L,crude_status$VE_U))
cat(sprintf("Adjust OR %.2f (%.2f-%.2f)  VE %.1f%% (%.1f to %.1f)  [adj: %s]\n", adj_status$OR,adj_status$OR_L,adj_status$OR_U,adj_status$VE,adj_status$VE_L,adj_status$VE_U, paste(adj_status$adj,collapse=", ")))
cat(sprintf("Repeatedly tested: %d | discordant/changing: %d\n", nrow(rep_tested), sum(rep_tested$transition %in% c("N->P","P->N","other"))))
cat("Tables -> ", TBL, "\nFigures -> ", FIG, "\nData -> ", OUT, "\n")
