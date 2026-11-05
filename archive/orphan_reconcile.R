# =============================================================================
#  orphan_reconcile.R  —  potential linkages for UNLINKED surveillance only
#
#  Reference = linked_dataset_MAIN (the confirmed linkage; NOT modified here).
#    orphan surveillance = 575 surveillance records whose participant is NOT in MAIN
#    orphan lab          = lab register patients whose patient_uid is NOT in MAIN
#  Then match ONLY (orphan surv x orphan lab): each orphan surveillance record ->
#  its own best potential lab candidate(s). No cross-product with linked records.
# =============================================================================
STD <- "C:/Users/hp/Documents/data/DashShiny_EVE_BackUp_Shared/data"
OUT <- "C:/Users/hp/Documents/data/DashShiny_EVE_BackUp_Shared/data"          # save next to the other linkage files
rc  <- function(p) read.csv(p, colClasses="character", check.names=FALSE, na.strings=character(0), encoding="UTF-8")
blank<-function(x){if(is.null(x))return(character(0));x[is.na(x)]<-"";trimws(x)}
norm <-function(s){s<-blank(s);t<-suppressWarnings(iconv(s,to="ASCII//TRANSLIT"));s<-ifelse(is.na(t),s,t);toupper(gsub("[^A-Z0-9 ]"," ",toupper(s)))}
norm <-function(s){s<-blank(s);t<-suppressWarnings(iconv(s,to="ASCII//TRANSLIT"));s<-ifelse(is.na(t),s,t);s<-toupper(s);gsub("\\s+"," ",trimws(gsub("[^A-Z0-9 ]"," ",s)))}
isRealId<-function(id){id<-toupper(blank(id));nzchar(id)&grepl("[0-9]",id)&!grepl("^(RAS|INCONNU|RDC|NA)$",id)}
pkey<-function(n1,n4,n5) ifelse(isRealId(n1),paste0("ID#",norm(n1)),paste0("NM#",norm(paste0(n4," ",n5))))
PLACE<-c("RAS","RDC","KAS","BLP","MVE","INC","INCONNU","UNKNOWN","KASAI","NOUVEAU","NE")
toks <-function(s){t<-strsplit(norm(s)," ")[[1]];t[nchar(t)>=3 & !(t%in%PLACE) & !grepl("^[0-9]+$",t)]}
cp   <-function(a,b){n<-min(nchar(a),nchar(b));if(n==0)return(0);k<-0;for(i in seq_len(n)){if(substr(a,i,i)==substr(b,i,i))k<-i else break};k}
partial<-function(a,b){if(a==b||nchar(a)<4||nchar(b)<4)return(FALSE);if(startsWith(a,b)||startsWith(b,a)||grepl(a,b,fixed=TRUE)||grepl(b,a,fixed=TRUE))return(TRUE);if(cp(a,b)>=4)return(TRUE);adist(a,b)[1,1]<=1}
ageN <-function(a){a<-blank(a);n<-suppressWarnings(as.numeric(sub(".*?([0-9]+(\\.[0-9]+)?).*","\\1",a)));n[!is.na(n)&(n<0|n>=110)]<-NA;n}
sx   <-function(s){s<-toupper(trimws(blank(s)));ifelse(substr(s,1,1)=="M"|s=="1","M",ifelse(substr(s,1,1)=="F"|s=="2","F",NA))}

sv   <- rc(file.path(STD,"raw_data","ebola_vaccine_effectiveness_surveillance.csv"))
main <- rc(file.path(STD,"linked_data","linked_dataset_MAIN.csv"))
merg <- rc(file.path(STD,"raw_data","merged_eve_lab_data.csv"))

sv$key0   <- pkey(sv$name_1, sv$name_4, sv$name_5)
main$key0 <- pkey(main$name_1, main$name_4, main$name_5)
linkedKeys <- unique(main$key0)
linkedUids <- unique(blank(main$lab_sample_id))

# ---- orphan sets ----
orphSurvIdx <- which(!(sv$key0 %in% linkedKeys))
orphLabIdx  <- which(!(blank(merg$patient_uid) %in% linkedUids))
cat("surveillance records:", nrow(sv), " | linked (in MAIN):", sum(sv$key0 %in% linkedKeys),
    " | ORPHAN surveillance:", length(orphSurvIdx), "\n")
cat("lab patients:", nrow(merg), " | linked (in MAIN):", sum(blank(merg$patient_uid) %in% linkedUids),
    " | ORPHAN lab:", length(orphLabIdx), "\n")

orphSurv <- data.frame(
  surv_key=sv$key0[orphSurvIdx], case_id=blank(sv$name_1)[orphSurvIdx],
  surname=blank(sv$name_4)[orphSurvIdx], firstname=blank(sv$name_5)[orphSurvIdx],
  sex=unname(c("1"="Male","2"="Female","3"="Unknown")[blank(sv$sex)[orphSurvIdx]]),
  age_years=blank(sv$age_years)[orphSurvIdx], health_zone=blank(sv$health_zone_1)[orphSurvIdx],
  health_area=blank(sv$health_area_1)[orphSurvIdx], onset=blank(sv$date_frist_sympt)[orphSurvIdx],
  pcr_surv=blank(sv$pcr_result)[orphSurvIdx], stringsAsFactors=FALSE)
orphLab <- data.frame(
  patient_uid=blank(merg$patient_uid)[orphLabIdx], lab_name=blank(merg$patient_name_clean)[orphLabIdx],
  sex=blank(merg$sex)[orphLabIdx], age_years=blank(merg$age_years)[orphLabIdx],
  health_zone=blank(merg$health_zone)[orphLabIdx], pcr_result=blank(merg$pcr_test_result)[orphLabIdx],
  onset=blank(merg$symptom_onset_date)[orphLabIdx], stringsAsFactors=FALSE)
write.csv(orphSurv, file.path(OUT,"orphan_surveillance.csv"), row.names=FALSE, na="")
write.csv(orphLab,  file.path(OUT,"orphan_lab.csv"),          row.names=FALSE, na="")

# ---- restricted matching: each orphan surveillance -> best orphan-lab candidate(s) ----
LN <- lapply(orphLab$lab_name, toks); Lfull<-norm(orphLab$lab_name)
Lsex<-sx(orphLab$sex); Lage<-ageN(orphLab$age_years)
scorePair <- function(si){
  st<-toks(paste(orphSurv$surname[si], orphSurv$firstname[si])); sf<-norm(paste(orphSurv$surname[si],orphSurv$firstname[si]))
  ssex<-sx(orphSurv$sex[si]); sage<-ageN(orphSurv$age_years[si])
  if(!length(st)) return(NULL)
  out<-list()
  for(j in seq_along(LN)){ lt<-LN[[j]]; if(!length(lt)) next
    ex<-sum(st %in% lt); shared<-st[st %in% lt]
    pa<-0; for(a in st[!(st%in%lt)]){for(b in lt[!(lt%in%st)]) if(partial(a,b)){pa<-pa+1;break}}
    sexok<-(!is.na(ssex)&&!is.na(Lsex[j])&&ssex==Lsex[j]); adiff<-if(!is.na(sage)&&!is.na(Lage[j])) abs(sage-Lage[j]) else NA
    ageok<-(!is.na(adiff)&&adiff<=5)
    fe<-nzchar(sf)&&sf==Lfull[j]
    tier<-if(fe&&sexok&&ageok)1 else if(ex>=2)2 else if(ex>=1&&pa>=1&&sexok&&ageok)3 else if(ex>=1||pa>=2)4 else NA
    if(is.na(tier)) next   # keep Tier 1-4; the dashboard presents Tier 3 and Tier 4 in SEPARATE panels
    out[[length(out)+1]]<-list(j=j,tier=tier,ex=ex,pa=pa,sexok=sexok,adiff=adiff,ageok=ageok,shared=paste(shared,collapse=" "))
  }
  if(!length(out)) return(NULL)
  out[order(sapply(out,function(o)o$tier), -sapply(out,function(o)o$ex), sapply(out,function(o)ifelse(is.na(o$adiff),99,o$adiff)))]
}
TIERLAB<-c("Tier 1 - Full-name exact","Tier 2 - Token partial","Tier 3 - Name + partial + demo","Tier 4 - Partial name parts")
rows<-list()
for(si in seq_len(nrow(orphSurv))){
  cand<-scorePair(si); if(is.null(cand)) next
  for(o in cand[seq_len(min(3,length(cand)))]){ j<-o$j
    rows[[length(rows)+1]]<-data.frame(
      surv_key=orphSurv$surv_key[si], surv_name=trimws(paste(orphSurv$surname[si],orphSurv$firstname[si])),
      surv_sex=orphSurv$sex[si], surv_age=orphSurv$age_years[si], surv_health_zone=orphSurv$health_zone[si],
      surv_onset=orphSurv$onset[si],
      candidate_tier=TIERLAB[o$tier], shared_tokens=o$shared,
      lab_patient_uid=orphLab$patient_uid[j], lab_name=orphLab$lab_name[j],
      lab_sex=unname(c("M"="Male","F"="Female")[Lsex[j]]), lab_age=orphLab$age_years[j],
      lab_health_zone=orphLab$health_zone[j], lab_pcr=orphLab$pcr_result[j],
      sex_agreement=ifelse(o$sexok,"True","False"),
      age_diff=ifelse(is.na(o$adiff),"",sprintf("%.1f",o$adiff)),
      age_within_5y=ifelse(o$ageok,"True","False"), stringsAsFactors=FALSE)
  }
}
pot <- if(length(rows)) do.call(rbind, rows) else data.frame()
write.csv(pot, file.path(OUT,"orphan_potential_linkages.csv"), row.names=FALSE, na="")

nsurv_with_cand <- length(unique(pot$surv_key))
cat("\nORPHAN potential linkages: ", nrow(pot), "candidate rows for", nsurv_with_cand,
    "of", nrow(orphSurv), "orphan surveillance records (",
    nrow(orphSurv)-nsurv_with_cand, "have NO orphan-lab candidate).\n")
cat("by best tier per surveillance record:\n")
best <- pot[!duplicated(pot$surv_key),]
print(table(best$candidate_tier))
cat("\nsaved: orphan_surveillance.csv, orphan_lab.csv, orphan_potential_linkages.csv in", OUT, "\n")
