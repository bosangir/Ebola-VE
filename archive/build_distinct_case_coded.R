# =============================================================================
#  build_distinct_case_coded.R
#  Builds a CLEAN, CODED, one-row-per-participant (distinct case) analytic file
#  from the confirmed linkage (linked_dataset_MAIN) + triangulated lab (wide),
#  keeping BOTH surveillance and laboratory demographics, and covering the
#  protocol §6 minimum required variables. Writes the dataset + a data dictionary.
# =============================================================================
suppressWarnings(suppressMessages({library(writexl)}))
build_distinct_case <- function(
    ROOT  = "C:/Users/hp/Documents/data/DashShiny_EVE_BackUp_Shared",
    WIDEF = "C:/Users/hp/Desktop/lab_data_triangulation/outputs/merged_eve_lab_wide.csv",
    OUT   = file.path(ROOT,"data","linked_data")){
STD <- file.path(ROOT,"data"); dir.create(OUT,showWarnings=FALSE,recursive=TRUE)
if(!file.exists(file.path(STD,"linked_data","linked_dataset_MAIN.csv")) || !file.exists(WIDEF)){
  message("build_distinct_case: MAIN or triangulated wide lab file missing — skipping (keeping any existing coded file)."); return(invisible(FALSE)) }
rc<-function(p) read.csv(p,colClasses="character",check.names=FALSE,na.strings=character(0),encoding="UTF-8")
blank<-function(x){if(is.null(x))return(character(0));x[is.na(x)]<-"";trimws(x)}
norm<-function(s){s<-blank(s);t<-suppressWarnings(iconv(s,to="ASCII//TRANSLIT"));s<-ifelse(is.na(t),s,t);toupper(gsub("\\s+"," ",trimws(s)))}
numv<-function(x){suppressWarnings(as.numeric(gsub("[^0-9.\\-]","",blank(x))))}
isRealId<-function(id){id<-toupper(blank(id));nzchar(id)&grepl("[0-9]",id)&!grepl("^(RAS|INCONNU|RDC|NA)$",id)}
pkey<-function(n1,n4,n5) ifelse(isRealId(n1),paste0("ID#",norm(n1)),paste0("NM#",norm(paste0(n4," ",n5))))
pdate<-function(x){x<-trimws(blank(x));out<-as.Date(rep(NA_real_,length(x)),origin="1970-01-01")
  for(f in c("%Y-%m-%d","%d-%b-%Y","%d-%b-%y","%d/%m/%Y","%d/%m/%y","%d-%m-%Y","%d.%m.%Y")){mm<-is.na(out)&nzchar(x);if(!any(mm))break;out[mm]<-suppressWarnings(as.Date(x[mm],format=f))}
  ser<-is.na(out)&grepl("^[0-9]{5}$",x); if(any(ser)) out[ser]<-as.Date(as.numeric(x[ser]),origin="1899-12-30"); as.character(out)}
g<-function(df,c) if(c %in% names(df)) blank(df[[c]]) else rep("",nrow(df))
YN <-c("1"="Yes","2"="No","99"="Unknown","0"="Unknown","3"="Other")
SEX<-c("1"="Male","2"="Female","3"="Unknown"); VAC<-c("1"="Vaccinated","2"="Not vaccinated","99"="Unknown")
VIT<-c("1"="Alive","2"="Dead","99"="Unknown"); TYP<-c("1"="rVSV-ZEBOV (Ervebo)","99"="Unknown type","2"="Ad26/MVA (Zabdeno/Mvabea)")

# ---- 1. load + dedup MAIN to distinct participants (canonical key) ----
m<-rc(file.path(STD,"linked_data","linked_dataset_MAIN.csv"))
m$pk<-pkey(m$name_1,m$name_4,m$name_5)
m$prio<-ifelse(g(m,"final_status")=="Reviewer-accepted",0,1)
m$tiern<-suppressWarnings(as.integer(sub(".*Tier\\s*([0-9]).*","\\1",g(m,"linkage_tier"))))
m<-m[order(m$pk,m$prio,m$tiern),]; mb<-m[!duplicated(m$pk),]
W<-rc(WIDEF); Wi<-match(g(mb,"lab_sample_id"), g(W,"patient_uid"))
wv<-function(c) if(c %in% names(W)) blank(W[[c]])[Wi] else rep("",nrow(mb))
resCols<-grep("^final_result_evd_[0-9]+$",names(W),value=TRUE); resN<-as.integer(sub(".*_","",resCols)); resCols<-resCols[order(resN)]; resN<-sort(resN)

# ---- 2. per-participant lab test history (uid-keyed, definitive Pos/Neg only) ----
labhist<-function(i){ wr<-Wi[i]; if(is.na(wr)) return(NULL)
  res<-blank(unlist(W[wr,resCols]))
  dt<-pdate(vapply(resN,function(k){cc<-paste0("test_result_date_",k); if(cc%in%names(W)) blank(W[[cc]][wr]) else ""},character(1)))
  keep<-res%in%c("Positive","Negative"); if(!any(keep)) return(NULL)
  res<-res[keep]; dt<-dt[keep]; o<-order(is.na(dt),dt); res<-res[o]; dt<-dt[o]
  pos<-res=="Positive"
  list(n=length(res),first_r=res[1],first_d=dt[1],last_r=res[length(res)],last_d=dt[length(dt)],
       mid=if(length(res)>2) paste(res[2:(length(res)-1)],collapse=";") else "",
       traj=paste(substr(res,1,1),collapse=""),any_pos=any(pos),
       dpos1=if(any(pos)) dt[which(pos)[1]] else "", dposL=if(any(pos)) dt[rev(which(pos))[1]] else "") }
H<-lapply(seq_len(nrow(mb)), labhist)
gv<-function(f,d="") vapply(H,function(h) if(is.null(h)||is.null(h[[f]])||is.na(h[[f]])) d else as.character(h[[f]]), character(1))

# ---- 3. assemble coded distinct-case dataset ----
age_surv<-numv(g(mb,"age_years")); age_lab<-numv(wv("age_years"))
sex_surv_c<-g(mb,"sex"); sex_lab_raw<-toupper(wv("sex")); sex_lab_c<-ifelse(substr(sex_lab_raw,1,1)=="M","1",ifelse(substr(sex_lab_raw,1,1)=="F","2",""))
sex_res_c<-ifelse(nzchar(sex_surv_c)&sex_surv_c%in%c("1","2"),sex_surv_c,sex_lab_c)
age_res<-ifelse(!is.na(age_surv),age_surv,age_lab)
n_doses<-(nzchar(pdate(g(mb,"date_vaccination1")))) + (nzchar(pdate(g(mb,"date_vaccination2"))))
n_doses[g(mb,"vaccin_mve")=="1" & n_doses==0]<-NA          # vaccinated but no date -> doses unknown
anyp<-gv("any_pos"); case_conf<-ifelse(anyp=="TRUE","1",ifelse(gv("n")=="","","0"))   # 1=confirmed case, 0=test-negative control, ''=no definitive result
# vital status: surveillance outcome (1=Alive,2=Dead,99=Unknown) OR lab final_status; Dead if either records a death
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
  # ---- laboratory test history: first / intermediate / last / any-positive ----
  n_tests=gv("n"),
  first_test_date=gv("first_d"), first_result=gv("first_r"), first_result_code=ifelse(gv("first_r")=="Positive","1",ifelse(gv("first_r")=="Negative","0","")),
  intermediate_results=gv("mid"),
  last_test_date=gv("last_d"), last_result=gv("last_r"), last_result_code=ifelse(gv("last_r")=="Positive","1",ifelse(gv("last_r")=="Negative","0","")),
  any_positive=ifelse(anyp=="TRUE","Yes",ifelse(gv("n")=="","","No")),
  date_first_positive=gv("dpos1"), date_last_positive=gv("dposL"),
  result_trajectory=gv("traj"),
  pcr_result_patient=wv("pcr_test_result"),
  # ---- case / control (test-negative design) ----
  case_confirmed=ifelse(case_conf=="1","Confirmed case (PCR+)",ifelse(case_conf=="0","Test-negative control (PCR-)","Indeterminate/none")),
  case_confirmed_code=case_conf,
  orthoebolavirus_species="",   # NOT recorded in source (see dictionary)
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
  linkage_tier=g(mb,"linkage_tier"), linkage_final_status=g(mb,"final_status"), age_diff=g(mb,"age_diff"),
  stringsAsFactors=FALSE, check.names=FALSE)
D[is.na(D)]<-""

# ---- 4. DATA DICTIONARY ----
dd<-function(v,l,ty,src,codes,note="") data.frame(variable=v,label=l,type=ty,source=src,values_codes=codes,notes=note,stringsAsFactors=FALSE)
DICT<-do.call(rbind,list(
 dd("study_id","Study identifier (created, sequential)","ID","Derived","EVE-0001…","Pseudonymous study ID per protocol §7"),
 dd("record_id","Surveillance case identifier","ID","Surveillance","free text","name_1; may be 'RAS' placeholder"),
 dd("lab_patient_uid","Laboratory patient identifier","ID","Laboratory","PAT_xxxxx","= merged/wide patient_uid"),
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
 dd("province","Province","text","Surveillance","","§6 geographic"),
 dd("health_zone","Health zone","text","Surveillance","","§6 geographic"),
 dd("health_area","Health area","text","Surveillance","","§6 geographic"),
 dd("village","Village","text","Surveillance","","§6 geographic (village_residence)"),
 dd("health_zone_lab","Health zone — laboratory","text","Laboratory","",""),
 dd("health_area_lab","Health area — laboratory","text","Laboratory","",""),
 dd("occupation_code","Occupation (coded)","code","Surveillance","1..13,99 (numeric)","NO decode key in source — request occupation codebook"),
 dd("hcw_frontline_role","HCW/frontline role (text)","text","Surveillance","e.g. INFIRMIER, HYGIENISTE",""),
 dd("hcw_frontline","Healthcare/frontline worker","categorical","Derived","Yes/Unknown","Yes if a health/frontline role recorded (type_personnel)"),
 dd("hcw_frontline_code","HCW/frontline code","code","Derived","1=Yes;99=Unknown",""),
 dd("prior_ebod","Prior documented EBOD history","categorical","Surveillance","Yes/No/Unknown","from 'infection'"),
 dd("prior_ebod_code","Prior EBOD code","code","Surveillance","1=Yes;2=No;99/0=Unknown",""),
 dd("vacc_status","Vaccination status","categorical","Surveillance","Vaccinated/Not vaccinated/Unknown","§6; register not available — surveillance field"),
 dd("vacc_status_code","Vaccination status code","code","Surveillance","1=Vaccinated;2=Not;99=Unknown",""),
 dd("vacc_product","Vaccine product","categorical","Surveillance","","type_vaccin1: 1=rVSV-ZEBOV, 99=Unknown"),
 dd("vacc_product_code","Vaccine product code","code","Surveillance","1;2;99",""),
 dd("n_doses","Number of doses","integer","Derived","0,1,2; ''=unknown","count of vaccination dates; blank if vaccinated w/o date"),
 dd("vacc_date1","Vaccination date — dose 1","date","Surveillance","YYYY-MM-DD",""),
 dd("vacc_date2","Vaccination date — dose 2","date","Surveillance","YYYY-MM-DD",""),
 dd("vacc_source_code","Source of vaccination info (coded)","code","Surveillance","1,2,3,99","NO decode key — request codebook"),
 dd("vacc_strategy_code","Vaccination strategy (coded)","code","Surveillance","1..5","NO decode key — ring vs targeted-geographic NOT resolvable yet"),
 dd("vacc_card","Vaccination card seen","categorical","Surveillance","Yes/No/Unknown",""),
 dd("vacc_card_code","Vaccination card code","code","Surveillance","1=Yes;2=No;99/0=Unknown",""),
 dd("onset_date","Date of symptom onset","date","Surveillance","YYYY-MM-DD","§6"),
 dd("specimen_date_first","Date of (first) specimen collection","date","Laboratory/Surv","YYYY-MM-DD","§6; first lab sample date, else surveillance specimen date"),
 dd("n_tests","Number of definitive RT-PCR tests","integer","Laboratory",">=1",""),
 dd("first_test_date","Date of first definitive test","date","Laboratory","",""),
 dd("first_result","First RT-PCR result","categorical","Laboratory","Positive/Negative",""),
 dd("first_result_code","First result code","code","Laboratory","1=Positive;0=Negative",""),
 dd("intermediate_results","Intermediate results (between first & last)","text","Laboratory","semicolon-separated",""),
 dd("last_test_date","Date of last definitive test","date","Laboratory","",""),
 dd("last_result","Last RT-PCR result","categorical","Laboratory","Positive/Negative",""),
 dd("last_result_code","Last result code","code","Laboratory","1=Positive;0=Negative",""),
 dd("any_positive","Any positive RT-PCR (ever)","categorical","Laboratory","Yes/No",""),
 dd("date_first_positive","Date of first positive","date","Laboratory","",""),
 dd("date_last_positive","Date of last positive","date","Laboratory","",""),
 dd("result_trajectory","Result sequence (P/N over time)","text","Laboratory","e.g. PPN",""),
 dd("pcr_result_patient","Patient-level RT-PCR result","categorical","Laboratory","Positive/Negative/…",""),
 dd("case_confirmed","Case / control (test-negative design)","categorical","Derived","Confirmed case (PCR+)/Test-negative control (PCR-)/Indeterminate","any-positive => case"),
 dd("case_confirmed_code","Case/control code","code","Derived","1=Case;0=Control;''=indeterminate",""),
 dd("orthoebolavirus_species","Identified orthoebolavirus species","categorical","Laboratory","(blank)","NOT RECORDED in source lab data; Zaire ebolavirus (EBOV) assumed for this outbreak — request species confirmation from INRB (§6)"),
 dd("vital_status","Vital status (alive/dead)","categorical","Surv+Lab","Alive/Dead/Unknown","§6 outcome/mortality; Dead if either source records a death"),
 dd("vital_status_code","Vital status code","code","Derived","1=Alive;2=Dead;99=Unknown",""),
 dd("date_of_death","Date of death","date","Laboratory","",""),
 dd("ever_hospitalised","Ever hospitalised (severity proxy)","categorical","Surveillance","Yes/No","§6 clinical severity"),
 dd("n_hospitalisations","Number of hospitalisations","integer","Surveillance",">=0",""),
 dd("contact_known","Contact known","categorical","Surveillance","Yes/No/Unknown","§6 exposure/contact"),
 dd("contact_known_code","Contact known code","code","Surveillance","1=Yes;2=No;99=Unknown",""),
 dd("contact_with_case","Contact with a (confirmed) case","categorical","Surveillance","Yes/No/Unknown","§6 contact type with confirmed EBOD case"),
 dd("contact_with_case_code","Contact with case code","code","Surveillance","1=Yes;2=No;99=Unknown",""),
 dd("funeral_participation","Funeral participation","categorical","Surveillance","Yes/No/Unknown","exposure"),
 dd("funeral_code","Funeral code","code","Surveillance","1=Yes;2=No;99=Unknown",""),
 dd("travel","Travel history","categorical","Surveillance","Yes/No/Unknown","exposure"),
 dd("travel_code","Travel code","code","Surveillance","1=Yes;2=No;99=Unknown",""),
 dd("linkage_tier","Linkage tier","categorical","Linkage","Tier 1..4",""),
 dd("linkage_final_status","Linkage acceptance status","categorical","Linkage","Auto-accepted/Reviewer-accepted",""),
 dd("age_diff","Age difference (surv vs lab)","numeric","Derived","years","")
))

write.csv(D, file.path(OUT,"distinct_case_coded.csv"), row.names=FALSE, na="")
write.csv(DICT, file.path(OUT,"distinct_case_data_dictionary.csv"), row.names=FALSE, na="")
write_xlsx(list(distinct_case_coded=D, data_dictionary=DICT), file.path(OUT,"distinct_case_coded.xlsx"))

cat("build_distinct_case: ", nrow(D), " distinct cases x ", ncol(D), " coded variables -> ", OUT, "\n", sep="")
invisible(TRUE)
}
# standalone run (Rscript build_distinct_case_coded.R) — writes to the analysis output folder
if (sys.nframe() == 0L) build_distinct_case(OUT = "C:/Users/hp/Documents/data/EVE_VE_Analysis/output")
