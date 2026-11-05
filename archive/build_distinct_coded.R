# =============================================================================
#  build_distinct_coded.R
#  Builds a CLEAN, CODED, one-row-per-participant (distinct case) ANALYTIC file
#  + a DATA DICTIONARY, directly from the confirmed distinct-case linkage export
#  (linked_dataset_MAIN_distinct_cases.csv) triangulated laboratory history
#  (merged_eve_lab_wide.csv, uid-keyed).  Keeps BOTH surveillance and laboratory
#  demographics and covers the protocol section-6 minimum required variables.
#
#  Adds analysis-ready fields so the VE analysis can run straight off this file:
#     - episode_result / episode_result_code  (first definitive PCR on/after onset)
#     - is_cross_record                        (orphan T3/T4 cross-record link flag)
#
#  USAGE:  "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" build_distinct_coded.R
#  OUT:    EVE_VE_Analysis/Distinct_CODED.csv/.xlsx  +  Distinct_CODED_data_dictionary.csv
# =============================================================================
suppressWarnings(suppressMessages({library(writexl)}))

ROOT  <- "C:/Users/hp/Documents/data/EVE_VE_Analysis"
DASH  <- "C:/Users/hp/Documents/data/DashShiny_EVE_BackUp_Shared/data"   # dashboard data (cleaned exports)
# ---- prefer the CLEANED linkage (duplicates merged, deletions & field-consolidation applied by the
#      dashboard) over the raw distinct-case export, so the analysis auto-reflects data cleaning ----
CAND <- c(file.path(ROOT, "cleaned_linkage.csv"),
          file.path(DASH, "linked_data", "cleaned_linkage.csv"),
          file.path(ROOT, "linked_dataset_MAIN_distinct_cases.csv"))
INF  <- CAND[file.exists(CAND)][1]
WIDEF <- "C:/Users/hp/Desktop/lab_data_triangulation/outputs/merged_eve_lab_wide.csv"
OUT   <- ROOT
if (is.na(INF)) stop("No linkage input found. Export 'Cleaned linkage' from the dashboard, or keep linked_dataset_MAIN_distinct_cases.csv.")
if(!file.exists(WIDEF)) stop("triangulated wide lab file missing: ", WIDEF)
cat("build_distinct_coded: source =", INF, "\n")

rc   <- function(p) read.csv(p, colClasses="character", check.names=FALSE, na.strings=character(0), encoding="UTF-8")
blank<- function(x){ if(is.null(x)) return(character(0)); x[is.na(x)]<-""; trimws(x) }
norm <- function(s){ s<-blank(s); t<-suppressWarnings(iconv(s,to="ASCII//TRANSLIT")); s<-ifelse(is.na(t),s,t); toupper(gsub("\\s+"," ",trimws(s))) }
numv <- function(x){ suppressWarnings(as.numeric(gsub("[^0-9.\\-]","",blank(x)))) }
isRealId<-function(id){ id<-toupper(blank(id)); nzchar(id)&grepl("[0-9]",id)&!grepl("^(RAS|INCONNU|RDC|NA)$",id) }
pkey <- function(n1,n4,n5) ifelse(isRealId(n1), paste0("ID#",norm(n1)), paste0("NM#",norm(paste0(n4," ",n5))))
pdate<- function(x){ x<-trimws(blank(x)); out<-as.Date(rep(NA_real_,length(x)),origin="1970-01-01")
  for(f in c("%Y-%m-%d","%d-%b-%Y","%d-%b-%y","%d/%m/%Y","%d/%m/%y","%d-%m-%Y","%d.%m.%Y")){ mm<-is.na(out)&nzchar(x); if(!any(mm))break; out[mm]<-suppressWarnings(as.Date(x[mm],format=f)) }
  ser<-is.na(out)&grepl("^[0-9]{5}$",x); if(any(ser)) out[ser]<-as.Date(as.numeric(x[ser]),origin="1899-12-30"); as.character(out) }
g    <- function(df,c) if(c %in% names(df)) blank(df[[c]]) else rep("", nrow(df))

YN <-c("1"="Yes","2"="No","99"="Unknown","0"="Unknown","3"="Other")
SEX<-c("1"="Male","2"="Female","3"="Unknown"); VAC<-c("1"="Vaccinated","2"="Not vaccinated","99"="Unknown")
VIT<-c("1"="Alive","2"="Dead","99"="Unknown"); TYP<-c("1"="rVSV-ZEBOV (Ervebo)","99"="Unknown type","2"="Ad26/MVA (Zabdeno/Mvabea)")

# ---- 1. load linkage; apply dashboard data-cleaning (deletions + field consolidations) ----
m <- rc(INF)
# apply the dashboard's persisted cleaning so the analysis reflects it even without a cleaned export
rj <- function(p){ if(file.exists(p)){ tryCatch(jsonlite::fromJSON(paste(readLines(p,warn=FALSE,encoding="UTF-8"),collapse=""),simplifyVector=FALSE), error=function(e) NULL) } else NULL }
suppressWarnings(suppressMessages(library(jsonlite)))
nId <- function(s) gsub("[^A-Z0-9]","",toupper(blank(s)))
del  <- rj(file.path(DASH,"deletions.json"));              del <- if(is.null(del)) character(0) else unlist(del)
corr <- rj(file.path(DASH,"record_corrections.json"));     corr <- if(is.null(corr)) list() else corr
if(length(del)){ keep <- !(g(m,"lab_sample_id") %in% del); cat(sprintf("  cleaning: dropped %d deleted lab record(s)\n", sum(!keep))); m <- m[keep,,drop=FALSE] }
if(length(corr)){                                    # consolidated/edited field values keyed by WP|uid|normName|normLabName
  labnm <- if("labreg_name" %in% names(m)) g(m,"labreg_name") else if("NAMES" %in% names(m)) g(m,"NAMES") else rep("",nrow(m))
  rid <- paste0("WP|", g(m,"lab_sample_id"), "|", nId(paste0(g(m,"name_4"),g(m,"name_5"))), "|", nId(labnm))
  nap <- 0
  for(i in seq_len(nrow(m))){ ce <- corr[[rid[i]]]; if(is.null(ce)) next
    for(k in names(ce)){ v <- ce[[k]]; if(k %in% names(m) && length(v) && nzchar(as.character(v)[1])){ m[i,k] <- as.character(v)[1]; nap <- nap+1 } } }
  if(nap) cat(sprintf("  cleaning: applied %d consolidated field value(s)\n", nap))
}
m$pk <- pkey(m$name_1, m$name_4, m$name_5)
# already distinct; keep a within-pair row over a cross-record row if a pkey repeats
m$prio <- ifelse(g(m,"match_type")=="within-pair", 0, 1)
m <- m[order(m$pk, m$prio), ]
mb <- m[!duplicated(m$pk), ]
cat(sprintf("build_distinct_coded: input rows=%d  distinct participants=%d\n", nrow(m), nrow(mb)))

# ---- triangulated lab history (uid-keyed) ----
W  <- rc(WIDEF); Wi <- match(g(mb,"lab_sample_id"), g(W,"patient_uid"))
wv <- function(c) if(c %in% names(W)) blank(W[[c]])[Wi] else rep("", nrow(mb))
resCols<-grep("^final_result_evd_[0-9]+$",names(W),value=TRUE); resN<-as.integer(sub(".*_","",resCols)); resCols<-resCols[order(resN)]; resN<-sort(resN)

onset_mb <- as.Date(pdate(g(mb,"date_frist_sympt")))

# ---- 2. per-participant lab test history (uid-keyed, definitive Pos/Neg only) ----
#     episode = first definitive specimen collected on/after symptom onset (else first).
labhist <- function(i){ wr<-Wi[i]; if(is.na(wr)) return(NULL)
  res<-blank(unlist(W[wr,resCols]))
  dt <- as.Date(pdate(vapply(resN,function(k){ cc<-paste0("test_result_date_",k); if(cc%in%names(W)) blank(W[[cc]][wr]) else "" }, character(1))))
  keep<-res%in%c("Positive","Negative"); if(!any(keep)) return(NULL)
  res<-res[keep]; dt<-dt[keep]; o<-order(is.na(dt),dt); res<-res[o]; dt<-dt[o]
  pos<-res=="Positive"
  ons<-onset_mb[i]
  epi<-if(!is.na(ons)) which(!is.na(dt) & dt>=ons)[1] else NA_integer_; if(is.na(epi)) epi<-1L
  list(n=length(res), first_r=res[1], first_d=dt[1], last_r=res[length(res)], last_d=dt[length(dt)],
       epi_r=res[epi], epi_d=dt[epi],
       mid=if(length(res)>2) paste(res[2:(length(res)-1)],collapse=";") else "",
       traj=paste(substr(res,1,1),collapse=""), any_pos=any(pos),
       dpos1=if(any(pos)) dt[which(pos)[1]] else as.Date(NA), dposL=if(any(pos)) dt[rev(which(pos))[1]] else as.Date(NA)) }
H<-lapply(seq_len(nrow(mb)), labhist)
gv<-function(f,d="") vapply(H,function(h){ if(is.null(h)||is.null(h[[f]])||all(is.na(h[[f]]))) return(d); v<-h[[f]]; if(inherits(v,"Date")) as.character(v) else as.character(v) }, character(1))

# ---- 3. assemble coded distinct-case dataset ----
age_surv<-numv(g(mb,"age_years")); age_lab<-numv(wv("age_years"))
sex_surv_c<-g(mb,"sex"); sex_lab_raw<-toupper(wv("sex")); sex_lab_c<-ifelse(substr(sex_lab_raw,1,1)=="M","1",ifelse(substr(sex_lab_raw,1,1)=="F","2",""))
sex_res_c<-ifelse(nzchar(sex_surv_c)&sex_surv_c%in%c("1","2"),sex_surv_c,sex_lab_c)
age_res<-ifelse(!is.na(age_surv),age_surv,age_lab)
n_doses<-(nzchar(pdate(g(mb,"date_vaccination1")))) + (nzchar(pdate(g(mb,"date_vaccination2"))))
n_doses[g(mb,"vaccin_mve")=="1" & n_doses==0]<-NA
anyp<-gv("any_pos")
# episode-defining case/control (protocol primary)
epi_r<-gv("epi_r"); epi_code<-ifelse(epi_r=="Positive","1",ifelse(epi_r=="Negative","0",""))
# any-positive case definition (used for the CODED case_confirmed field)
case_conf<-ifelse(anyp=="TRUE","1",ifelse(gv("n")=="","","0"))
# ---- site-verified RT-PCR result overrides (result_overrides.json, uid-keyed) win over derived ----
ROV <- rj(file.path(DASH,"result_overrides.json")); if(is.null(ROV)) ROV<-list()
ovv <- vapply(g(mb,"lab_sample_id"), function(u){ v<-ROV[[u]]; if(is.null(v)) "" else as.character(v)[1] }, character(1))
sel<-ovv=="Positive"; epi_r[sel]<-"Positive"; anyp[sel]<-"TRUE"
sel<-ovv=="Negative"; epi_r[sel]<-"Negative"; anyp[sel]<-"FALSE"
sel<-ovv=="Invalid";  epi_r[sel]<-"";         anyp[sel]<-""       # invalid -> indeterminate, excluded
epi_code<-ifelse(epi_r=="Positive","1",ifelse(epi_r=="Negative","0",""))
case_conf<-ifelse(anyp=="TRUE","1",ifelse(anyp=="FALSE","0",case_conf))
if(sum(nzchar(ovv))) cat(sprintf("  applied %d site-verified result override(s)\n", sum(nzchar(ovv))))
result_overridden<-ifelse(nzchar(ovv), ovv, "")
# vital status
surv_out<-unname(VIT[g(mb,"outcome")]); lab_fs<-wv("final_status")
lab_dead<-grepl("dead|deced|deces|decede",lab_fs,ignore.case=TRUE); lab_alive<-grepl("alive|vivant",lab_fs,ignore.case=TRUE)
vital<-ifelse((surv_out%in%"Dead")|lab_dead,"Dead",ifelse((surv_out%in%"Alive")|lab_alive,"Alive","Unknown"))

D<-data.frame(
  study_id       = sprintf("EVE-%04d", seq_len(nrow(mb))),
  record_id      = g(mb,"name_1"),
  lab_patient_uid= g(mb,"lab_sample_id"),
  firstname      = g(mb,"name_5"),
  surname        = g(mb,"name_4"),
  # ---- demographics: surveillance, lab, resolved ----
  age_surv=age_surv, age_lab=age_lab, age=age_res,
  age_group = cut(age_res,c(-Inf,5,17,44,Inf),labels=c("0-5","6-17","18-44","45+")),
  age_group_code = as.integer(cut(age_res,c(-Inf,5,17,44,Inf))),
  sex_surv = unname(SEX[sex_surv_c]), sex_surv_code = sex_surv_c,
  sex_lab  = unname(SEX[sex_lab_c]),  sex_lab_code  = sex_lab_c,
  sex      = unname(SEX[sex_res_c]),  sex_code      = sex_res_c,
  sex_agreement = g(mb,"sex_agreement"),
  # ---- geography (surveillance + lab) ----
  province=g(mb,"province_1"), health_zone=g(mb,"health_zone_1"), health_area=g(mb,"health_area_1"), village=g(mb,"village_residence"),
  health_zone_lab=wv("health_zone"), health_area_lab=wv("health_area"),
  # ---- occupation / HCW-frontline ----
  occupation_code=g(mb,"occupation"), hcw_frontline_role=g(mb,"type_personnel"),
  hcw_frontline=ifelse(nzchar(g(mb,"type_personnel")) & toupper(g(mb,"type_personnel"))!="RAS","Yes","Unknown"),
  hcw_frontline_code=ifelse(nzchar(g(mb,"type_personnel")) & toupper(g(mb,"type_personnel"))!="RAS","1","99"),
  # ---- prior EBOD history ----
  prior_ebod = unname(YN[g(mb,"infection")]), prior_ebod_code=g(mb,"infection"),
  # ---- vaccination ----
  vacc_status=unname(VAC[g(mb,"vaccin_mve")]), vacc_status_code=g(mb,"vaccin_mve"),
  vacc_product=unname(TYP[g(mb,"type_vaccin1")]), vacc_product_code=g(mb,"type_vaccin1"),
  n_doses=n_doses,
  vacc_date1=pdate(g(mb,"date_vaccination1")), vacc_date2=pdate(g(mb,"date_vaccination2")),
  vacc_source_code=g(mb,"source_vacccination"), vacc_strategy_code=g(mb,"vaccine_strategy"),
  vacc_card=unname(YN[g(mb,"carte")]), vacc_card_code=g(mb,"carte"),
  # ---- clinical dates ----
  onset_date=pdate(g(mb,"date_frist_sympt")),
  specimen_date_first=ifelse(nzchar(gv("first_d")), gv("first_d"), pdate(g(mb,"specimen_collection_date"))),
  # ---- laboratory test history ----
  n_tests=gv("n"),
  first_test_date=gv("first_d"), first_result=gv("first_r"), first_result_code=ifelse(gv("first_r")=="Positive","1",ifelse(gv("first_r")=="Negative","0","")),
  intermediate_results=gv("mid"),
  last_test_date=gv("last_d"), last_result=gv("last_r"), last_result_code=ifelse(gv("last_r")=="Positive","1",ifelse(gv("last_r")=="Negative","0","")),
  # ---- episode-defining outcome (protocol PRIMARY) ----
  episode_test_date=gv("epi_d"), episode_result=epi_r, episode_result_code=epi_code, result_overridden=result_overridden,
  any_positive=ifelse(anyp=="TRUE","Yes",ifelse(gv("n")=="","","No")),
  date_first_positive=gv("dpos1"), date_last_positive=gv("dposL"),
  result_trajectory=gv("traj"),
  pcr_result_patient=wv("pcr_test_result"),
  # ---- case / control (any-positive definition) ----
  case_confirmed=ifelse(case_conf=="1","Confirmed case (PCR+)",ifelse(case_conf=="0","Test-negative control (PCR-)","Indeterminate/none")),
  case_confirmed_code=case_conf,
  orthoebolavirus_species="",
  # ---- clinical severity + outcome / mortality ----
  vital_status=vital, vital_status_code=ifelse(vital=="Alive","1",ifelse(vital=="Dead","2","99")),
  date_of_death=pdate(wv("date_of_death")),
  ever_hospitalised=g(mb,"ever_hospitalised"), n_hospitalisations=g(mb,"n_hospitalisations"),
  # ---- exposure / contact ----
  contact_known=unname(YN[g(mb,"contact_connu")]), contact_known_code=g(mb,"contact_connu"),
  contact_with_case=unname(YN[g(mb,"contact")]), contact_with_case_code=g(mb,"contact"),
  funeral_participation=unname(YN[g(mb,"funerailles")]), funeral_code=g(mb,"funerailles"),
  travel=unname(YN[g(mb,"voyage")]), travel_code=g(mb,"voyage"),
  # ---- linkage metadata ----
  linkage_tier=g(mb,"linkage_tier"), linkage_final_status=g(mb,"final_status"),
  match_type=g(mb,"match_type"),
  is_cross_record=ifelse(g(mb,"match_type")=="cross-record" | grepl("Cross",g(mb,"final_status")),"Yes","No"),
  age_diff=g(mb,"age_diff"),
  stringsAsFactors=FALSE, check.names=FALSE)
D[is.na(D)]<-""

# ---- 4. DATA DICTIONARY ----
dd<-function(v,l,ty,src,codes,note="") data.frame(variable=v,label=l,type=ty,source=src,values_codes=codes,notes=note,stringsAsFactors=FALSE)
DICT<-do.call(rbind,list(
 dd("study_id","Study identifier (created, sequential)","ID","Derived","EVE-0001…","Pseudonymous study ID per protocol section 7"),
 dd("record_id","Surveillance case identifier","ID","Surveillance","free text","name_1; may be 'RAS' placeholder"),
 dd("lab_patient_uid","Laboratory patient identifier","ID","Laboratory","PAT_xxxxx","= triangulated wide patient_uid (join key)"),
 dd("firstname","First name","text","Surveillance","","name_5 — identifier, remove before sharing"),
 dd("surname","Surname / second name","text","Surveillance","","name_4 — identifier, remove before sharing"),
 dd("age_surv","Age (years) — surveillance","numeric","Surveillance",">=0",""),
 dd("age_lab","Age (years) — laboratory","numeric","Laboratory",">=0",""),
 dd("age","Age (years) — resolved (surv preferred)","numeric","Derived",">=0",""),
 dd("age_group","Age group","categorical","Derived","0-5;6-17;18-44;45+",""),
 dd("age_group_code","Age group code","integer","Derived","1=0-5;2=6-17;3=18-44;4=45+",""),
 dd("sex_surv","Sex — surveillance","categorical","Surveillance","see sex_surv_code",""),
 dd("sex_surv_code","Sex code — surveillance","code","Surveillance","1=Male;2=Female;3=Unknown",""),
 dd("sex_lab","Sex — laboratory","categorical","Laboratory","",""),
 dd("sex_lab_code","Sex code — laboratory","code","Laboratory","1=Male;2=Female",""),
 dd("sex","Sex — resolved","categorical","Derived","",""),
 dd("sex_code","Sex code — resolved","code","Derived","1=Male;2=Female;3=Unknown",""),
 dd("sex_agreement","Surv vs lab sex agree","boolean","Derived","True/False",""),
 dd("province","Province","text","Surveillance","","section 6 geographic"),
 dd("health_zone","Health zone","text","Surveillance","","section 6 geographic"),
 dd("health_area","Health area","text","Surveillance","","section 6 geographic"),
 dd("village","Village","text","Surveillance","","section 6 geographic (village_residence)"),
 dd("health_zone_lab","Health zone — laboratory","text","Laboratory","",""),
 dd("health_area_lab","Health area — laboratory","text","Laboratory","",""),
 dd("occupation_code","Occupation (coded)","code","Surveillance","1..13,99 (numeric)","NO decode key in source — request occupation codebook"),
 dd("hcw_frontline_role","HCW/frontline role (text)","text","Surveillance","e.g. INFIRMIER, HYGIENISTE",""),
 dd("hcw_frontline","Healthcare/frontline worker","categorical","Derived","Yes/Unknown","Yes if a health/frontline role recorded (type_personnel)"),
 dd("hcw_frontline_code","HCW/frontline code","code","Derived","1=Yes;99=Unknown",""),
 dd("prior_ebod","Prior documented EBOD history","categorical","Surveillance","Yes/No/Unknown","from 'infection'"),
 dd("prior_ebod_code","Prior EBOD code","code","Surveillance","1=Yes;2=No;99/0=Unknown",""),
 dd("vacc_status","Vaccination status","categorical","Surveillance","Vaccinated/Not vaccinated/Unknown","section 6; register not available — surveillance field"),
 dd("vacc_status_code","Vaccination status code","code","Surveillance","1=Vaccinated;2=Not;99=Unknown","PRIMARY exposure"),
 dd("vacc_product","Vaccine product","categorical","Surveillance","","type_vaccin1: 1=rVSV-ZEBOV, 99=Unknown"),
 dd("vacc_product_code","Vaccine product code","code","Surveillance","1;2;99",""),
 dd("n_doses","Number of doses","integer","Derived","0,1,2; ''=unknown","count of vaccination dates; blank if vaccinated w/o date"),
 dd("vacc_date1","Vaccination date — dose 1","date","Surveillance","YYYY-MM-DD",""),
 dd("vacc_date2","Vaccination date — dose 2","date","Surveillance","YYYY-MM-DD",""),
 dd("vacc_source_code","Source of vaccination info (coded)","code","Surveillance","1,2,3,99","NO decode key — request codebook"),
 dd("vacc_strategy_code","Vaccination strategy (coded)","code","Surveillance","1..5","NO decode key — ring vs targeted-geographic NOT resolvable yet"),
 dd("vacc_card","Vaccination card seen","categorical","Surveillance","Yes/No/Unknown",""),
 dd("vacc_card_code","Vaccination card code","code","Surveillance","1=Yes;2=No;99/0=Unknown",""),
 dd("onset_date","Date of symptom onset","date","Surveillance","YYYY-MM-DD","section 6"),
 dd("specimen_date_first","Date of (first) specimen collection","date","Laboratory/Surv","YYYY-MM-DD","section 6; first lab sample date, else surveillance specimen date"),
 dd("n_tests","Number of definitive RT-PCR tests","integer","Laboratory",">=1",""),
 dd("first_test_date","Date of first definitive test","date","Laboratory","",""),
 dd("first_result","First RT-PCR result","categorical","Laboratory","Positive/Negative",""),
 dd("first_result_code","First result code","code","Laboratory","1=Positive;0=Negative",""),
 dd("intermediate_results","Intermediate results (between first & last)","text","Laboratory","semicolon-separated",""),
 dd("last_test_date","Date of last definitive test","date","Laboratory","",""),
 dd("last_result","Last RT-PCR result","categorical","Laboratory","Positive/Negative",""),
 dd("last_result_code","Last result code","code","Laboratory","1=Positive;0=Negative",""),
 dd("episode_test_date","Date of episode-defining specimen","date","Derived","","first definitive specimen on/after onset (else first)"),
 dd("episode_result","Episode-defining RT-PCR result","categorical","Derived","Positive/Negative","PROTOCOL PRIMARY outcome"),
 dd("episode_result_code","Episode-defining result code","code","Derived","1=Positive(case);0=Negative(control)","PRIMARY analytic outcome Y"),
 dd("result_overridden","Site-verified result override","categorical","Site/Lab","Positive/Negative/Invalid/blank","non-blank = the episode result was set by a site 'Verify lab result' response, overriding the derived lab history"),
 dd("any_positive","Any positive RT-PCR (ever)","categorical","Laboratory","Yes/No",""),
 dd("date_first_positive","Date of first positive","date","Laboratory","",""),
 dd("date_last_positive","Date of last positive","date","Laboratory","",""),
 dd("result_trajectory","Result sequence (P/N over time)","text","Laboratory","e.g. PPN",""),
 dd("pcr_result_patient","Patient-level RT-PCR result","categorical","Laboratory","Positive/Negative/…",""),
 dd("case_confirmed","Case / control (any-positive definition)","categorical","Derived","Confirmed case (PCR+)/Test-negative control (PCR-)/Indeterminate","any-positive => case"),
 dd("case_confirmed_code","Case/control code (any-positive)","code","Derived","1=Case;0=Control;''=indeterminate",""),
 dd("orthoebolavirus_species","Identified orthoebolavirus species","categorical","Laboratory","(blank)","NOT RECORDED in source; Zaire ebolavirus (EBOV) assumed — request species confirmation (section 6)"),
 dd("vital_status","Vital status (alive/dead)","categorical","Surv+Lab","Alive/Dead/Unknown","section 6 outcome/mortality; Dead if either source records a death"),
 dd("vital_status_code","Vital status code","code","Derived","1=Alive;2=Dead;99=Unknown",""),
 dd("date_of_death","Date of death","date","Laboratory","",""),
 dd("ever_hospitalised","Ever hospitalised (severity proxy)","categorical","Surveillance","Yes/No","section 6 clinical severity"),
 dd("n_hospitalisations","Number of hospitalisations","integer","Surveillance",">=0",""),
 dd("contact_known","Contact known","categorical","Surveillance","Yes/No/Unknown","section 6 exposure/contact"),
 dd("contact_known_code","Contact known code","code","Surveillance","1=Yes;2=No;99=Unknown",""),
 dd("contact_with_case","Contact with a (confirmed) case","categorical","Surveillance","Yes/No/Unknown","section 6 contact type with confirmed EBOD case"),
 dd("contact_with_case_code","Contact with case code","code","Surveillance","1=Yes;2=No;99=Unknown",""),
 dd("funeral_participation","Funeral participation","categorical","Surveillance","Yes/No/Unknown","exposure"),
 dd("funeral_code","Funeral code","code","Surveillance","1=Yes;2=No;99=Unknown",""),
 dd("travel","Travel history","categorical","Surveillance","Yes/No/Unknown","exposure"),
 dd("travel_code","Travel code","code","Surveillance","1=Yes;2=No;99=Unknown",""),
 dd("linkage_tier","Linkage tier","categorical","Linkage","Tier 1..4 / Cross-record",""),
 dd("linkage_final_status","Linkage acceptance status","categorical","Linkage","Auto-accepted/Reviewer-accepted/Cross-accepted",""),
 dd("match_type","Match type","categorical","Linkage","within-pair/cross-record",""),
 dd("is_cross_record","Cross-record (orphan T3/T4) link","categorical","Linkage","Yes/No","Yes = lower-confidence orphan reconciliation link; exclude in sensitivity"),
 dd("age_diff","Age difference (surv vs lab)","numeric","Derived","years","")
))

write.csv(D,    file.path(OUT,"Distinct_CODED.csv"), row.names=FALSE, na="")
write.csv(DICT, file.path(OUT,"Distinct_CODED_data_dictionary.csv"), row.names=FALSE, na="")
write_xlsx(list(Distinct_CODED=D, data_dictionary=DICT), file.path(OUT,"Distinct_CODED.xlsx"))

cat(sprintf("build_distinct_coded: %d distinct cases x %d coded variables -> %s\n", nrow(D), ncol(D), OUT))
cat(sprintf("  episode-defining: cases(PCR+)=%d  controls(PCR-)=%d  indeterminate=%d\n",
    sum(D$episode_result_code=="1"), sum(D$episode_result_code=="0"), sum(D$episode_result_code=="")))
cat(sprintf("  cross-record (orphan) links flagged: %d\n", sum(D$is_cross_record=="Yes")))
