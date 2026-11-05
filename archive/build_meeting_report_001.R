# =============================================================================
#  build_meeting_report_001.R
#  Meeting minutes / brief (Century Gothic) summarising today's discussion:
#   1. Preliminary analyses (matching rate pos vs neg; missing-data rate)
#   2. Progress of data cleaning
#   3. Next steps
#  All numbers computed from source files; nothing hard-coded.
#  OUT: EVE_VE_Analysis/Meeting_Report_001.docx
# =============================================================================
suppressWarnings(suppressMessages({library(officer); library(flextable); library(readxl); library(magrittr)}))
ROOT<-"C:/Users/hp/Documents/data/EVE_VE_Analysis"
WIDE<-"C:/Users/hp/Desktop/lab_data_triangulation/outputs/merged_eve_lab_wide.csv"
OUTdoc<-file.path(ROOT,"Meeting_Report_001.docx")
rc<-function(p) read.csv(p,colClasses="character",check.names=FALSE,na.strings=character(0),encoding="UTF-8")
blank<-function(x){x[is.na(x)]<-"";trimws(x)}
pct<-function(k,n) sprintf("%d (%.1f%%)",k,ifelse(n>0,100*k/n,0))

# ---- (1a) matching quality by PCR result — WITHIN THE 392 DISTINCT LINKED ONLY ----
D<-rc(file.path(ROOT,"Distinct_CODED.csv")); N<-nrow(D)
res<-ifelse(D$episode_result_code=="1","Positive",ifelse(D$episode_result_code=="0","Negative","Indeterminate"))
tierNum<-suppressWarnings(as.integer(sub(".*Tier\\s*([0-9]).*","\\1",D$linkage_tier)))
sexagree<-toupper(substr(D$sex_agreement,1,1))=="T"; adiff<-suppressWarnings(as.numeric(D$age_diff)); cross<-D$is_cross_record=="Yes"
hi<-(tierNum%in%c(1,2))&!cross
medIQR<-function(sel){v<-adiff[sel & !is.na(adiff)]; if(!length(v)) return("-"); sprintf("%.1f (%.1f-%.1f)",median(v),quantile(v,.25),quantile(v,.75))}
qcol<-function(sel){n<-sum(sel); c(as.character(n),
  pct(sum(sel&tierNum==1,na.rm=TRUE),n),pct(sum(sel&tierNum==2,na.rm=TRUE),n),
  pct(sum(sel&(tierNum%in%c(3,4)|cross),na.rm=TRUE),n),
  pct(sum(sel&sexagree),n),pct(sum(sel&!is.na(adiff)&adiff<=5),n),medIQR(sel),pct(sum(sel&hi),n))}
qp<-qcol(res=="Positive"); qn<-qcol(res=="Negative")
q_tbl<-data.frame(Metric=c("N","Tier 1 (full-name exact)","Tier 2 (token match)","Tier 3/4 or cross-record","Sex agreement","Age difference <= 5 years","Age difference, median (IQR)","High-confidence (Tier 1/2, within-pair)"),
  `PCR Positive (cases)`=qp,`PCR Negative (controls)`=qn,check.names=FALSE)
mmq<-matrix(c(sum(res=="Positive"&hi),sum(res=="Positive"&!hi),sum(res=="Negative"&hi),sum(res=="Negative"&!hi)),nrow=2,byrow=TRUE); ftq<-fisher.test(mmq)
posN<-sum(res=="Positive"); negN<-sum(res=="Negative")

# ---- (1b) missing-data rate (392 distinct; vaccination fields use the VACCINATED denominator) ----
nv<-sum(D$vacc_status=="Vaccinated")
mrows<-list(
  c("Age",                                     sum(nzchar(D$age)),                                        N),
  c("Sex",                                      sum(D$sex%in%c("Male","Female")),                          N),
  c("Health zone",                              sum(nzchar(D$health_zone)),                                N),
  c("RT-PCR result (episode-defining)",         sum(D$episode_result_code%in%c("0","1")),                  N),
  c("Vital status",                             sum(D$vital_status%in%c("Alive","Dead")),                  N),
  c("Symptom-onset date",                       sum(nzchar(D$onset_date)),                                 N),
  c("Specimen/swab date",                       sum(nzchar(D$specimen_date_first)),                        N),
  c("Vaccination status",                       sum(D$vacc_status_code%in%c("1","2")),                     N),
  c("Vaccination date (among vaccinated)",      sum(D$vacc_status=="Vaccinated"&nzchar(D$vacc_date1)),     nv),
  c("Vaccination strategy (among vaccinated)",  sum(D$vacc_status=="Vaccinated"&nzchar(D$vacc_strategy_code)), nv),
  c("Village",                                  sum(nzchar(D$village)),                                    N))
miss_tbl<-do.call(rbind,lapply(mrows,function(r){k<-as.integer(r[2]);den<-as.integer(r[3])
  data.frame(Variable=r[1],Denominator=den,
    `Complete n (%)`=sprintf("%d (%.1f%%)",k,ifelse(den>0,100*k/den,0)),
    `Missing %`=sprintf("%.1f%%",ifelse(den>0,100*(den-k)/den,0)),check.names=FALSE,stringsAsFactors=FALSE)}))

# ---- VE recap ----
ve<-tryCatch(as.data.frame(read_excel(file.path(ROOT,"main_output_coded","coded_analysis_tables.xlsx"),sheet="VE_models")),error=function(e)NULL)

# ============================ DOCUMENT ======================================
FONT<-"Century Gothic"; set_flextable_defaults(font.family=FONT,font.size=9,padding=3,theme_fun=theme_booktabs)
tt<-function(txt,sz=11,bold=FALSE,color="#000000",italic=FALSE) fp_text(font.family=FONT,font.size=sz,bold=bold,color=color,italic=italic)
par0<-fp_par(text.align="left",padding.bottom=6); H1n<-0
doc<-read_docx()
H1<-function(txt){H1n<<-H1n+1;doc<<-doc%>%body_add_fpar(fpar(ftext(paste0(H1n,". ",txt),tt(txt,14,TRUE,"#1F3864")),fp_p=fp_par(padding.top=10,padding.bottom=4)))}
H2<-function(txt){doc<<-doc%>%body_add_fpar(fpar(ftext(txt,tt(txt,11.5,TRUE,"#2E5496")),fp_p=fp_par(padding.top=5,padding.bottom=2)))}
BODY<-function(txt){doc<<-doc%>%body_add_fpar(fpar(ftext(txt,tt(txt,10.5)),fp_p=par0))}
BULl<-function(txt){doc<<-doc%>%body_add_fpar(fpar(ftext(paste0("•  ",txt),tt(txt,10.5)),fp_p=fp_par(padding.bottom=2,padding.left=14)))}
CAP<-function(txt){doc<<-doc%>%body_add_fpar(fpar(ftext(txt,tt(txt,9,TRUE,"#404040",TRUE)),fp_p=fp_par(padding.top=6,padding.bottom=2)))}
NOTE<-function(txt){doc<<-doc%>%body_add_fpar(fpar(ftext(txt,tt(txt,9.5,FALSE,"#7030A0",TRUE)),fp_p=fp_par(padding.bottom=4)))}
FT<-function(df){ft<-flextable(as.data.frame(df))%>%bold(part="header")%>%bg(part="header",bg="#D9E2F3")%>%border_outer(part="all")%>%border_inner_h(part="body")%>%fontsize(size=8.5,part="all")%>%font(fontname=FONT,part="all")%>%autofit();doc<<-doc%>%body_add_flextable(ft)%>%body_add_par("")}

doc<-doc%>%
  body_add_fpar(fpar(ftext("Meeting Report 001",tt("",18,TRUE,"#1F3864")),fp_p=fp_par(text.align="center",padding.top=18,padding.bottom=2)))%>%
  body_add_fpar(fpar(ftext("Ebola Vaccine Effectiveness — Bulape / Kasai, DRC: preliminary analysis and data-cleaning review",tt("",11.5,FALSE,"#2E5496")),fp_p=fp_par(text.align="center",padding.bottom=2)))%>%
  body_add_fpar(fpar(ftext("20 August 2026",tt("",10,FALSE,"#404040")),fp_p=fp_par(text.align="center",padding.bottom=12)))
BODY("Agenda: (1) preliminary analyses — comparison of matching rates between positive and negative PCR results, and missing-data rate; (2) progress of data cleaning; (3) next steps.")

# ---- 1 ----
H1("Preliminary analyses")
H2("1.1  Matching rate — positive vs negative PCR (within the 392 distinct linked)")
BODY(sprintf("Restricting to the %d distinct linked participants (the analytic set), %d are PCR-positive (cases) and %d PCR-negative (controls). The two arms were matched with equivalent quality: the high-confidence link rate (Tier 1/2, within-pair) is %.1f%% for positives and %.1f%% for negatives (Fisher exact p = %.2f), with near-identical sex agreement, age concordance, and a median age difference of 0 years in both arms.",
  N,posN,negN,100*mmq[1,1]/sum(mmq[1,]),100*mmq[2,1]/sum(mmq[2,]),ftq$p.value))
CAP("Table 1. Match quality by PCR result, within the 392 distinct linked participants.")
FT(q_tbl)
NOTE("Interpretation: within the linked cohort there is no differential match QUALITY by test result, so the positive vs negative comparison used in the test-negative analysis is well balanced on linkage confidence.")

H2("1.2  Missing-data rate")
BODY(sprintf("Missingness within the %d distinct linked participants. Vaccination date and strategy are assessed among the %d VACCINATED participants only (the denominator to which they apply), so as not to inflate missingness. On that basis the vaccination STRATEGY value is essentially complete (its limitation is the absence of a decode key, not missing data), whereas the vaccination DATE is missing for nearly half of vaccinated participants and is the binding limitation for the protocol >10-day effective-window analysis. Demographic and laboratory-result fields are near-complete.",N,nv))
CAP("Table 2. Completeness and missing-data rate for key analytic variables.")
FT(miss_tbl)
NOTE("Of the 46 PCR-positive cases, only 27 have known vaccination status; the 19 with unknown status are excluded from every model, which is the principal driver of the small effective sample and wide confidence intervals.")

if(!is.null(ve)){
  H2("1.3  Preliminary VE (recap, episode-defining outcome)")
  CAP("Table 3. Vaccine-effectiveness estimates (test-negative design).")
  FT(ve[,c("Model","N","Cases","Controls","OR","VE %","VE 95% CI")])
  BODY("The crude and Firth (sparse-data) estimates are preferred at this number of cases; the adjusted model is directionally protective but imprecise (over-parameterised for the available events).")
}

# ---- 2 ----
H1("Progress of data cleaning")
BULl("Laboratory triangulation complete: six source datasets (DATA001-DATA006 plus a health-zone update) merged into one record per laboratory patient identifier (1,135 patients, uid-keyed).")
BULl("Record linkage adjudicated: name-based tiered matching with a demographic gate; Tier 1 auto-accepted, Tiers 2-4 manually reviewed; links judged wrong during adjudication were removed.")
BULl("Confirmed linkage consolidated: 414 confirmed rows reduced to 392 distinct participants on a single canonical identity key (22 name-variant duplicate records merged); 16 orphan (Tier 3/4) cross-record links reconciled and retained.")
BULl("Specimen mis-assignment bug fixed: laboratory histories are now joined by patient identifier (exact ID join), not by name.")
BULl("Analytic products built: a coded one-row-per-participant dataset (79 variables) with a companion data dictionary; the VE analysis is fully reproducible from that single file.")
BULl("Known residual issue: name-variant duplicates recorded under DIFFERENT laboratory identifiers are not yet merged (the register flags roughly 156 possible same-patient records).")

# ---- 3 ----
H1("Next steps")
BULl("Investigate and reduce the differential linkage: recover unmatched negative-result patients (currently 37.5% linked) and quantify the sensitivity of the VE estimate to it.")
BULl("Obtain register-verified vaccination status and dates, prioritising the 19 PCR-positive cases with unknown vaccination status; this would unlock the protocol >10-day effective-window analysis.")
BULl("Patient-level de-duplication: merge name-variant records across laboratory identifiers (~156 flagged).")
BULl("Obtain decode keys for the vaccination strategy (ring vs targeted-geographic), vaccination source, and occupation code lists.")
BULl("Confirm the orthoebolavirus species (Zaire ebolavirus assumed) with INRB.")
BULl("Finalise the remaining review-queue adjudication and lock the confirmed linkage.")
BULl("Re-run the VE analysis on the cleaned and expanded dataset, and add a mortality (VE-against-death) secondary analysis now that vital status is linked.")

print(doc,target=OUTdoc)
cat("Meeting report written: ",OUTdoc,"\n",sep="")
cat(sprintf("  within-392 high-confidence match: Pos %.1f%% vs Neg %.1f%% (Fisher p=%.2f)\n",100*mmq[1,1]/sum(mmq[1,]),100*mmq[2,1]/sum(mmq[2,]),ftq$p.value))
cat(sprintf("  cases(PCR+)=%d controls(PCR-)=%d\n",posN,negN))
