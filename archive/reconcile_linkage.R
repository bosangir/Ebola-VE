# =============================================================================
#  reconcile_linkage.R  —  IDENTIFICATION step (before building the new analytic set)
#
#  Reconciles two linkage sources into one participant-level roster:
#    * linked_dataset_MAIN.csv  = confirmed linkage from the dashboard (reference)
#    * analytic_participant_level.xlsx = analytic sample; rows the reviewer
#      highlighted YELLOW are outright-wrong linkages.
#
#  Rules:
#    - All MAIN participants are kept (MAIN is the trusted reference).
#    - Analytic participants missing from MAIN keep their analytic linkage,
#      UNLESS flagged yellow -> set UNLINKED (no other potential linkage).
#    - A yellow row whose MAIN link is the SAME record -> UNLINKED (manual review
#      overrides MAIN). A yellow row where MAIN gives a DIFFERENT record -> use
#      MAIN (ORANGE). Participants only in MAIN -> ORANGE (additional).
#  Colours in the output workbook:  ORANGE = additional/alternative from MAIN;
#      LIGHT YELLOW = de-linked (was wrong);  no fill = confirmed/kept/unlinked.
# =============================================================================
suppressMessages({library(openxlsx); library(xml2)})
MAIN <- "C:/Users/hp/Documents/data/DashShiny_EVE_BackUp_Shared/data/linked_data/linked_dataset_MAIN.csv"
AX   <- "C:/Users/hp/Documents/data/EVE_VE_Analysis/output/analytic_participant_level.xlsx"
MERG <- "C:/Users/hp/Documents/data/DashShiny_EVE_BackUp_Shared/data/raw_data/merged_eve_lab_data.csv"
OUTD <- "C:/Users/hp/Documents/data/EVE_VE_Analysis/output"

blank<-function(x){x[is.na(x)]<-"";trimws(as.character(x))}
norm <-function(s){s<-blank(s);t<-suppressWarnings(iconv(s,to="ASCII//TRANSLIT"));s<-ifelse(is.na(t),s,t);toupper(gsub("\\s+"," ",trimws(s)))}
isRealId<-function(id){id<-toupper(blank(id));nzchar(id)&grepl("[0-9]",id)&!grepl("^(RAS|INCONNU|RDC|NA)$",id)}
pkey<-function(n1,n4,n5) ifelse(isRealId(n1),paste0("ID#",norm(n1)),paste0("NM#",norm(paste0(n4," ",n5))))
tiern<-function(x) suppressWarnings(as.integer(sub(".*Tier\\s*([0-9]).*","\\1",x)))

# ---- 1. detect YELLOW-highlighted rows in the analytic xlsx (parse fills) ----
detect_yellow <- function(xlsx){
  tmp<-tempfile(); dir.create(tmp); unzip(xlsx, exdir=tmp)
  ws<-list.files(file.path(tmp,"xl","worksheets"),pattern="sheet.*xml$",full.names=TRUE)[1]
  st<-read_xml(file.path(tmp,"xl","styles.xml"))
  L1<-function(n,nm) xml_find_first(n,paste0(".//*[local-name()='",nm,"']"))
  fills<-xml_find_all(st,"./*[local-name()='fills']/*[local-name()='fill']")
  frgb<-vapply(fills,function(f){fg<-L1(f,"fgColor");if(is.na(fg)) NA_character_ else toupper(xml_attr(fg,"rgb"))},character(1))
  xfs<-xml_find_all(st,"./*[local-name()='cellXfs']/*[local-name()='xf']")
  xfill<-vapply(xfs,function(x){v<-xml_attr(x,"fillId");if(is.na(v))0L else as.integer(v)},integer(1))
  xrgb<-frgb[xfill+1]
  ys<-which(!is.na(xrgb)&grepl("FFFF00$",xrgb))-1L
  sh<-read_xml(ws); rows<-xml_find_all(sh,".//*[local-name()='row']")
  out<-integer(0)
  for(r in rows){rn<-as.integer(xml_attr(r,"r"))
    cs<-xml_find_all(r,"./*[local-name()='c']"); sv<-suppressWarnings(as.integer(xml_attr(cs,"s")))
    rs<-suppressWarnings(as.integer(xml_attr(r,"s"))); if(!is.na(rs)) sv<-c(sv,rs)
    if(any(sv %in% ys, na.rm=TRUE)) out<-c(out,rn)}
  out[out>=2]-1                       # -> data-row indices (row 1 = header)
}

# ---- 2. load sources --------------------------------------------------------
m <- read.csv(MAIN, colClasses="character", check.names=FALSE, na.strings=character(0))
m$key0 <- pkey(m$name_1,m$name_4,m$name_5); m$tiern <- tiern(m$linkage_tier)
m$prio <- ifelse(blank(m$final_status)=="Reviewer-accepted",0,1)
mo <- m[order(m$key0,m$prio,m$tiern),]; dupN<-ave(rep(1,nrow(mo)),mo$key0,FUN=length)
mb <- mo[!duplicated(mo$key0),]; mb$main_dup_n<-as.integer(dupN[!duplicated(mo$key0)])
dat <- read.xlsx(AX, sheet=1, colNames=TRUE); dat$yellow<-FALSE; dat$yellow[detect_yellow(AX)]<-TRUE
merg <- read.csv(MERG, colClasses="character", check.names=FALSE, na.strings=character(0))
uid2name <- setNames(blank(merg$patient_name_clean), blank(merg$patient_uid))
ykeys <- unique(dat$key0[dat$yellow])

# ---- 3. build union roster --------------------------------------------------
getA<-function(k,c){if(!c%in%names(dat))return(NA);i<-match(k,dat$key0);v<-if(is.na(i))NA else dat[[c]][i];if(length(v)==0)NA else v}
getM<-function(k,c){if(!c%in%names(mb)) return(NA);i<-match(k,mb$key0); v<-if(is.na(i))NA else mb[[c]][i]; if(length(v)==0)NA else v}
allkeys <- union(mb$key0, dat$key0)
rows <- lapply(allkeys, function(k){
  inM<-k%in%mb$key0; inA<-k%in%dat$key0; yel<-k%in%ykeys
  if(inA){scase<-getA(k,"surv_name");ssex<-getA(k,"sex");sage<-getA(k,"surv_age");szone<-getA(k,"health_zone");sarea<-getA(k,"health_area");scid<-getA(k,"case_id")}
  else   {scase<-trimws(paste(getM(k,"name_4"),getM(k,"name_5")));ssex<-getM(k,"sex");sage<-getM(k,"age_years");szone<-getM(k,"health_zone_1");sarea<-getM(k,"health_area_1");scid<-getM(k,"name_1")}
  labA<-blank(getA(k,"lab_name")); auid<-blank(getA(k,"patient_uid"))
  linkedA<-inA && !is.na(getA(k,"link_method")) && getA(k,"link_method")!="Unlinked" && !yel
  if(inM){
    muid<-blank(getM(k,"lab_sample_id")); rn<-unname(uid2name[muid])
    if(inA && yel && nzchar(auid) && auid==muid){
      src<-"Analytic override";lab<-"";uid<-"";tier<-"Unlinked";fs<-"";adiff<-"";sagr<-"";evd<-""
      status<-"UNLINKED - flagged wrong; MAIN holds the SAME link (overridden)";color<-"lightyellow"
    } else {
      src<-"MAIN (confirmed)";lab<-ifelse(!is.na(rn)&&nzchar(rn),rn,labA)
      tier<-getM(k,"linkage_tier");fs<-getM(k,"final_status");uid<-muid
      adiff<-getM(k,"age_diff");sagr<-getM(k,"sex_agreement");evd<-getM(k,"evd_result_final")
      if(!inA)      {status<-"MAIN additional (not in analytic)";color<-"orange"}
      else if(yel)  {status<-"MAIN alternative linkage (analytic was wrong; MAIN differs)";color<-"orange"}
      else          {status<-ifelse(linkedA && nzchar(auid) && auid!=muid,"Confirmed by MAIN (analytic had a different lab id)","Confirmed (MAIN and analytic agree)");color<-"none"}
    }
  } else {
    tier<-getA(k,"link_method");fs<-"";uid<-auid;adiff<-getA(k,"age_diff_years");sagr<-getA(k,"sex_agreement");evd<-getA(k,"episode_result")
    if(yel){src<-"Analytic";lab<-"";status<-"UNLINKED - analytic linkage was wrong (yellow), no MAIN match";color<-"lightyellow";tier<-"Unlinked";uid<-"";evd<-""}
    else if(linkedA){src<-"Analytic (kept)";lab<-labA;status<-"Analytic linkage kept (not in MAIN)";color<-"none"}
    else{src<-"Unlinked";lab<-"";status<-"Unlinked (no linkage in either source)";color<-"none";tier<-"Unlinked"}
  }
  data.frame(key0=k,linkage_status=status,color=color,linkage_source=src,
    surv_case_id=blank(scid),surv_name=blank(scase),surv_sex=blank(ssex),surv_age=blank(sage),
    surv_health_zone=blank(szone),surv_health_area=blank(sarea),
    lab_name=blank(lab),analytic_lab_name=labA,lab_id=blank(uid),linkage_tier=blank(tier),
    linkage_final_status=blank(fs),age_diff=blank(adiff),sex_agreement=blank(sagr),evd_result=blank(evd),
    main_duplicate_n=ifelse(inM,getM(k,"main_dup_n"),NA),stringsAsFactors=FALSE)
})
R <- do.call(rbind, rows)
R <- R[order(match(R$color,c("orange","none","lightyellow")),R$linkage_status,R$surv_name),]
R$row_no<-seq_len(nrow(R)); R<-R[,c("row_no",setdiff(names(R),"row_no"))]

# ---- 4. write coloured workbook + csv ---------------------------------------
wb<-createWorkbook(); addWorksheet(wb,"reconciled_roster")
disp<-R[,setdiff(names(R),"color")]
writeData(wb,1,disp,headerStyle=createStyle(textDecoration="bold",fgFill="#D9E2F3",border="Bottom"))
for(i in which(R$color=="orange"))      addStyle(wb,1,createStyle(fgFill="#FFC000"),rows=i+1,cols=1:ncol(disp),gridExpand=TRUE,stack=TRUE)
for(i in which(R$color=="lightyellow")) addStyle(wb,1,createStyle(fgFill="#FFF2CC",textDecoration="italic"),rows=i+1,cols=1:ncol(disp),gridExpand=TRUE,stack=TRUE)
setColWidths(wb,1,cols=1:ncol(disp),widths="auto"); freezePane(wb,1,firstRow=TRUE)
addWorksheet(wb,"legend")
writeData(wb,2,data.frame(Colour=c("ORANGE","LIGHT YELLOW","No fill"),
  Meaning=c("Additional/alternative linkage from linked_dataset_MAIN (participant not in analytic, or analytic link was wrong and MAIN gives a DIFFERENT lab record).",
            "Set to UNLINKED because the analytic link was flagged wrong (yellow): no MAIN match, or MAIN held the SAME wrong link so your manual review overrides it.",
            "Confirmed (MAIN & analytic agree), analytic linkage kept (not in MAIN), or genuinely unlinked in both.")))
saveWorkbook(wb, file.path(OUTD,"reconciled_linkage_roster.xlsx"), overwrite=TRUE)
write.csv(R[,setdiff(names(R),"color")], file.path(OUTD,"reconciled_linkage_roster.csv"), row.names=FALSE, na="")

cat("Reconciled roster:", nrow(R), "participants\n"); print(table(R$linkage_status))
cat("\nColours -> orange:",sum(R$color=="orange")," light-yellow(unlinked):",sum(R$color=="lightyellow")," none:",sum(R$color=="none"),"\n")
cat("MAIN participants (deduped):",nrow(mb)," | MAIN duplicate rows collapsed:",nrow(m)-nrow(mb),"\n")
cat("Yellow (wrong) analytic linkages:",length(ykeys),"\n")

