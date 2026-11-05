# =============================================================================
#  build_site_query_workbook.R
#  ONE Excel workbook of data-quality / site queries for the EVE study, covering
#  BOTH the MAIN-linked (392) and the unlinked/orphan records, plus candidate
#  extra linkages, so the site can (a) clear the 201 open queries, (b) clean the
#  linked data, and (c) continue confirming extra possible linkages.
#
#  Sheets: 1 Summary | 2 Open_Queries (201) | 3 Resolved_Queries (137)
#          4 Linked_DQ_Checks | 5 Unlinked_Surveillance | 6 Possible_Linkages
#          7 Duplicate_Review
#  OUT: EVE_VE_Analysis/EVE_Site_Query_and_Cleaning_Report_20082026.xlsx
# =============================================================================
suppressWarnings(suppressMessages({library(openxlsx); library(jsonlite)}))
STD <- "C:/Users/hp/Documents/data/DashShiny_EVE_BackUp_Shared/data"
ANL <- "C:/Users/hp/Documents/data/EVE_VE_Analysis"
OUT <- file.path(ANL,"EVE_Site_Query_and_Cleaning_Report_20082026.xlsx")
rc  <- function(p) read.csv(p,colClasses="character",check.names=FALSE,na.strings=character(0),encoding="UTF-8")
blank<-function(x){if(is.null(x))return(character(0));x[is.na(x)]<-"";trimws(x)}
normId<-function(s){s<-blank(s);gsub("[^A-Z0-9]","",toupper(s))}
numv<-function(x){suppressWarnings(as.numeric(gsub("[^0-9.\\-]","",blank(x))))}
asD <-function(x){suppressWarnings(as.Date(ifelse(blank(x)=="",NA,blank(x))))}
pdate<-function(x){x<-trimws(blank(x));out<-as.Date(rep(NA_real_,length(x)),origin="1970-01-01")
  for(f in c("%Y-%m-%d","%d-%b-%Y","%d-%b-%y","%d/%m/%Y","%d/%m/%y","%d-%m-%Y","%d.%m.%Y")){mm<-is.na(out)&nzchar(x);if(!any(mm))break;out[mm]<-suppressWarnings(as.Date(x[mm],format=f))}
  ser<-is.na(out)&grepl("^[0-9]{5}$",x); if(any(ser)) out[ser]<-as.Date(as.numeric(x[ser]),origin="1899-12-30"); out}

# ---------------------------------------------------------------- 1. QUERIES ---
V <- rc(file.path(STD,"violations.csv"))
V$Query_ID <- paste0("CT|",blank(V$category),"|",normId(V$record_id),"|",normId(V$record_name),"|",normId(V$rule))
cs <- fromJSON(paste(readLines(file.path(STD,"cleaning_status.json"),warn=FALSE,encoding="UTF-8"),collapse=""))
resolvedKeys <- names(cs)
V$resolved <- V$Query_ID %in% resolvedKeys
sevRank <- c("Critical"=1,"Major"=2)[blank(V$severity)]; sevRank[is.na(sevRank)] <- 3
V$Priority <- ifelse(blank(V$severity)=="","Minor / review",blank(V$severity))
qbase <- data.frame(
  Query_ID = V$Query_ID, Priority = V$Priority, Source = blank(V$source),
  Category = blank(V$category), Record_ID = blank(V$record_id), Name = blank(V$record_name),
  Health_zone = blank(V$health_zone), Issue = blank(V$rule),
  Value_1 = blank(V$value1), Value_2 = blank(V$value2),
  Action_needed = blank(V$needed), sev=sevRank, stringsAsFactors=FALSE, check.names=FALSE)
open  <- qbase[!V$resolved,]; open <- open[order(open$sev, open$Source),]
open$Status <- "OPEN"; open$Site_response <- ""; open$Resolution_date <- ""
open$sev <- NULL
res <- qbase[V$resolved,]; res$sev<-NULL
# attach the site's recorded correction
ci <- match(res$Query_ID, resolvedKeys)
getcs<-function(f) vapply(ci,function(k){ e<-cs[[k]]; v<-if(is.list(e)) e[[f]] else NULL; if(is.null(v)||length(v)==0) "" else as.character(v)[1]},character(1))
res$Status <- getcs("status"); res$Corrected_field<-getcs("corrected_field"); res$Corrected_value<-getcs("corrected_value"); res$Site_notes<-getcs("notes")

# ------------------------------------------------ 2. LINKED (392) DQ CHECKS ----
D <- rc(file.path(ANL,"Distinct_CODED.csv"))
onset<-asD(D$onset_date); vdt<-asD(D$vacc_date1); spec<-asD(D$specimen_date_first)
v2o<-as.numeric(onset-vdt); o2s<-as.numeric(spec-onset); age<-numv(D$age); adiff<-numv(D$age_diff)
nm<-paste(blank(D$surname),blank(D$firstname))
mkiss<-function(sel,cat,desc,val,act,pri){ if(!any(sel)) return(NULL)
  data.frame(Priority=pri,study_id=D$study_id[sel],Record_ID=D$record_id[sel],Name=nm[sel],lab_uid=D$lab_patient_uid[sel],
    Health_zone=D$health_zone[sel],Issue=cat,Description=desc,Current_value=val[sel],Action_needed=act,
    stringsAsFactors=FALSE,check.names=FALSE) }
L <- do.call(rbind,list(
  mkiss(D$episode_result_code=="" ,"Indeterminate RT-PCR","Linked but no definitive positive/negative result — cannot classify case/control",D$last_result,"Locate a definitive RT-PCR result or confirm indeterminate","Critical"),
  mkiss(!is.na(v2o)&v2o<0,"Vaccination after onset","Vaccination date is AFTER symptom onset (implausible for protection)",ifelse(is.na(v2o),"",paste0(v2o," d")),"Verify vaccination and onset dates","Critical"),
  mkiss(!is.na(o2s)&o2s<0,"Specimen before onset","Specimen collected BEFORE symptom onset (date error)",ifelse(is.na(o2s),"",paste0(o2s," d")),"Verify specimen and onset dates","Critical"),
  mkiss(toupper(substr(blank(D$sex_agreement),1,1))=="F","Sex disagreement","Surveillance sex does not match laboratory sex",paste0("surv ",D$sex_surv," / lab ",D$sex_lab),"Confirm correct sex; verify the link","Major"),
  mkiss(!is.na(adiff)&adiff>5,"Age discrepancy >5y","Surveillance vs laboratory age differ by more than 5 years",ifelse(is.na(adiff),"",paste0(adiff," y")),"Confirm age; verify the link","Major"),
  mkiss(is.na(onset),"Missing onset date","Symptom-onset date missing",blank(D$onset_date),"Obtain symptom-onset date from source form","Major"),
  mkiss(D$vacc_status_code=="99"|D$vacc_status_code=="","Unknown vaccination status","Vaccination status unknown — excluded from VE analysis",D$vacc_status,"Obtain vaccination status (register/card)","Major"),
  mkiss(D$vacc_status=="Vaccinated"&is.na(vdt),"Missing vaccination date","Vaccinated but vaccination date missing — blocks >10-day window",blank(D$vacc_date1),"Obtain vaccination date from card/register","Major"),
  mkiss(!is.na(o2s)&o2s>30,"Onset-to-swab >30d","Long delay from onset to specimen (>30 days)",ifelse(is.na(o2s),"",paste0(o2s," d")),"Confirm dates","Minor / review"),
  mkiss(D$is_cross_record=="Yes","Cross-record (orphan) link","Lower-confidence Tier 3/4 orphan link included in the analytic set",D$linkage_tier,"Adjudicate the link (accept/reject)","Minor / review")
))
if(is.null(L)) L<-data.frame(Priority=character(),Record_ID=character())
Lord<-c("Critical"=1,"Major"=2,"Minor / review"=3)[L$Priority]; L<-L[order(Lord),]

# ------------------------------------------- 3. UNLINKED SURVEILLANCE (orphans) --
OS <- rc(file.path(STD,"orphan_surveillance.csv"))
POT<- rc(file.path(STD,"orphan_potential_linkages.csv"))
hasCand <- blank(OS$surv_key) %in% blank(POT$surv_key)
os_on<-pdate(OS$onset)
U <- data.frame(
  Priority = ifelse(hasCand,"Review candidate link","No lab link"),
  Record_ID = blank(OS$case_id), Surname=blank(OS$surname), Firstname=blank(OS$firstname),
  Sex=blank(OS$sex), Age=blank(OS$age_years), Health_zone=blank(OS$health_zone),
  Onset=blank(OS$onset), Surv_PCR=blank(OS$pcr_surv),
  Has_candidate_lab_link = ifelse(hasCand,"Yes — see Possible_Linkages","No candidate found"),
  Missing_onset = ifelse(is.na(os_on),"Yes",""),
  Action_needed = ifelse(hasCand,"Review the candidate lab link in sheet 6","Search lab register / confirm never sampled"),
  stringsAsFactors=FALSE, check.names=FALSE)
U <- U[order(U$Has_candidate_lab_link=="No candidate found", U$Health_zone),]

# ------------------------------------------------ 4. POSSIBLE EXTRA LINKAGES ----
PL <- data.frame(
  Decision = "", Surv_key=blank(POT$surv_key), Surv_name=blank(POT$surv_name), Surv_sex=blank(POT$surv_sex),
  Surv_age=blank(POT$surv_age), Surv_health_zone=blank(POT$surv_health_zone), Surv_onset=blank(POT$surv_onset),
  Candidate_tier=blank(POT$candidate_tier), Shared_tokens=blank(POT$shared_tokens),
  Lab_uid=blank(POT$lab_patient_uid), Lab_name=blank(POT$lab_name), Lab_sex=blank(POT$lab_sex),
  Lab_age=blank(POT$lab_age), Lab_health_zone=blank(POT$lab_health_zone), Lab_PCR=blank(POT$lab_pcr),
  Sex_agreement=blank(POT$sex_agreement), Age_diff=blank(POT$age_diff), Age_within_5y=blank(POT$age_within_5y),
  stringsAsFactors=FALSE, check.names=FALSE)
tord<-c("Tier 2"=1,"Tier 3"=2,"Tier 4"=3); PL$to<-3; for(k in names(tord)) PL$to[grepl(k,PL$Candidate_tier)]<-tord[[k]]
PL<-PL[order(PL$to, PL$Surv_name),]; PL$to<-NULL

# ------------------------------------------------ 5. DUPLICATE REVIEW (linked) --
key<-normId(paste0(blank(D$surname),blank(D$firstname)))
dup<-key[duplicated(key) | duplicated(key,fromLast=TRUE)] ; dsel<-key %in% unique(dup) & nzchar(key)
DUP <- if(any(dsel)) data.frame(Priority="Possible duplicate participant",
  Name_key=key[dsel], study_id=D$study_id[dsel], Record_ID=D$record_id[dsel], Name=nm[dsel],
  lab_uid=D$lab_patient_uid[dsel], Age=D$age[dsel], Sex=D$sex[dsel], Health_zone=D$health_zone[dsel],
  Episode_result=D$episode_result[dsel], Vital_status=D$vital_status[dsel],
  Action_needed="Confirm same vs different person; merge duplicate laboratory identifiers",
  stringsAsFactors=FALSE,check.names=FALSE) else data.frame(Note="No same-name duplicates within the 392 linked set")
if("Name_key" %in% names(DUP)) DUP<-DUP[order(DUP$Name_key),]

# ---------------------------------------------------------------- SUMMARY -------
summ <- data.frame(check.names=FALSE, Section=c(
  "SITE DATA QUERIES (from the data-quality engine)","  Total queries raised","  Resolved / corrected by the site","  OPEN — remaining to address",
  "    of which Critical","    of which Major","    of which Minor / name-variant / review",
  "  Open by source: Surveillance","  Open by source: Laboratory",
  "MAIN-LINKED DATA CLEANING (392 participants)","  Record-level issues flagged (sheet 4)",
  "UNLINKED SURVEILLANCE (orphans)","  Orphan surveillance records (sheet 5)","    with a candidate lab link to review","    with no candidate found",
  "POSSIBLE EXTRA LINKAGES to confirm (sheet 6)","DUPLICATE PARTICIPANTS to review (sheet 7)"),
  Count=c(NA, nrow(V), sum(V$resolved), sum(!V$resolved),
    sum(!V$resolved & blank(V$severity)=="Critical"), sum(!V$resolved & blank(V$severity)=="Major"), sum(!V$resolved & !blank(V$severity)%in%c("Critical","Major")),
    sum(!V$resolved & blank(V$source)=="Surveillance"), sum(!V$resolved & blank(V$source)=="Lab"),
    NA, nrow(L), NA, nrow(U), sum(hasCand), sum(!hasCand), nrow(PL), if("Name_key"%in%names(DUP)) nrow(DUP) else 0))
summ$Count <- ifelse(is.na(summ$Count),"",as.character(summ$Count))

# ---------------------------------------------------------------- WRITE ---------
wb<-createWorkbook()
hdr<-createStyle(fontColour="#FFFFFF",fgFill="#1F3864",textDecoration="bold",halign="left",border="TopBottomLeftRight",wrapText=TRUE)
titleSt<-createStyle(fontSize=13,textDecoration="bold",fontColour="#1F3864")
critSt<-createStyle(fgFill="#F8CBAD"); majSt<-createStyle(fgFill="#FFE699"); minSt<-createStyle(fgFill="#E2EFDA")
addSheet<-function(name,df,title,priCol=NULL){
  addWorksheet(wb,name,tabColour="#2E5496"); writeData(wb,name,title,startRow=1); addStyle(wb,name,titleSt,rows=1,cols=1)
  writeData(wb,name,df,startRow=3,headerStyle=hdr)
  if(nrow(df)>0) addFilter(wb,name,rows=3,cols=1:ncol(df))
  freezePane(wb,name,firstActiveRow=4)
  setColWidths(wb,name,cols=1:ncol(df),widths="auto")
  if(!is.null(priCol) && priCol%in%names(df) && nrow(df)>0){ pc<-which(names(df)==priCol); r0<-4
    pv<-df[[priCol]]
    for(i in seq_len(nrow(df))){ st<-if(grepl("Critical",pv[i]))critSt else if(grepl("Major",pv[i]))majSt else if(nzchar(pv[i]))minSt else NULL
      if(!is.null(st)) addStyle(wb,name,st,rows=r0+i-1,cols=pc,gridExpand=TRUE) } }
}
# Summary
addWorksheet(wb,"Summary",tabColour="#C00000"); writeData(wb,"Summary","EVE — Site Query & Data-Cleaning Report (20 Aug 2026)",startRow=1); addStyle(wb,"Summary",titleSt,rows=1,cols=1)
writeData(wb,"Summary","Prepared for the site to (1) clear the 201 open queries, (2) clean the MAIN-linked data, and (3) confirm extra possible linkages.",startRow=2)
writeData(wb,"Summary",summ,startRow=4,headerStyle=hdr); setColWidths(wb,"Summary",cols=1:2,widths=c(60,12)); freezePane(wb,"Summary",firstActiveRow=5)
addSheet("Open_Queries",open,"Open data queries for the site to address (remaining)",priCol="Priority")
addSheet("Resolved_Queries",res,"Queries already resolved/corrected by the site (audit trail)")
addSheet("Linked_DQ_Checks",L,"MAIN-linked (392): record-level data-quality checks to clean",priCol="Priority")
addSheet("Unlinked_Surveillance",U,"Unlinked (orphan) surveillance records — need a lab link / result",priCol="Priority")
addSheet("Possible_Linkages",PL,"Candidate extra linkages to confirm (fill Decision: Accept / Reject)")
addSheet("Duplicate_Review",DUP,"Possible duplicate participants in the linked set (merge/confirm)",priCol="Priority")
saveWorkbook(wb,OUT,overwrite=TRUE)
# strip the dangling drawing/vml/printerSettings rels this openxlsx build injects
fixpy<-file.path(ANL,"fix_xlsx_rels.py")
if(file.exists(fixpy)) try(system2("python",c(shQuote(fixpy),shQuote(OUT)),stdout=TRUE,stderr=TRUE),silent=TRUE)

cat("Workbook written:",OUT,"\n")
cat(sprintf("  Queries: total %d | resolved %d | OPEN %d (Critical %d, Major %d, other %d)\n",
  nrow(V),sum(V$resolved),sum(!V$resolved),
  sum(!V$resolved & blank(V$severity)=="Critical"),sum(!V$resolved & blank(V$severity)=="Major"),
  sum(!V$resolved & !blank(V$severity)%in%c("Critical","Major"))))
cat(sprintf("  Linked DQ issues %d | Unlinked surv %d (cand %d) | Possible linkages %d | Dup rows %d\n",
  nrow(L),nrow(U),sum(hasCand),nrow(PL), if("Name_key"%in%names(DUP)) nrow(DUP) else 0))
