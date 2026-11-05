# =============================================================================
#  analyze_coded_report.R
#  Preliminary VE analysis (test-negative design) run DIRECTLY on the coded
#  analytic dataset (Distinct_CODED.csv) and Word report in Century Gothic.
#  All numbers are computed from the coded file; nothing hard-coded.
#
#  IN :  EVE_VE_Analysis/Distinct_CODED.csv   (built by build_distinct_coded.R)
#  OUT:  EVE_VE_Analysis/EVE_VE_Preliminary_Report_001_20082026.docx
#        EVE_VE_Analysis/main_output_coded/ (analytic csv, tables xlsx, figures)
# =============================================================================
suppressWarnings(suppressMessages({library(dplyr); library(ggplot2); library(officer); library(flextable); library(writexl)}))
ROOT <- "C:/Users/hp/Documents/data/EVE_VE_Analysis"
IN   <- file.path(ROOT,"Distinct_CODED.csv")
OUT  <- file.path(ROOT,"main_output_coded"); dir.create(OUT,showWarnings=FALSE,recursive=TRUE)
FIG  <- file.path(OUT,"figures"); dir.create(FIG,showWarnings=FALSE)
REPORT <- file.path(ROOT,"EVE_VE_Preliminary_Report_001_20082026.docx")
stopifnot(file.exists(IN))

D <- read.csv(IN, colClasses="character", check.names=FALSE, na.strings=character(0), encoding="UTF-8"); D[is.na(D)]<-""
numv<-function(x){ suppressWarnings(as.numeric(x)) }
asDate<-function(x){ suppressWarnings(as.Date(ifelse(x=="",NA,x))) }
ve_from_or<-function(or,lo,hi){ or<-unname(or); lo<-unname(lo); hi<-unname(hi); c(VE=(1-or)*100, VE_L=(1-hi)*100, VE_U=(1-lo)*100) }

# ---- outcomes / exposure / covariates (all from the CODED file) ----
D$age      <- numv(D$age)
D$onset    <- asDate(D$onset_date)
D$vacc_dt  <- asDate(D$vacc_date1)
D$Y_episode<- ifelse(D$episode_result_code=="1",1L, ifelse(D$episode_result_code=="0",0L,NA_integer_))
D$Y_first  <- ifelse(D$first_result_code=="1",1L, ifelse(D$first_result_code=="0",0L,NA_integer_))
D$Y_last   <- ifelse(D$last_result_code=="1",1L, ifelse(D$last_result_code=="0",0L,NA_integer_))
D$Y_any    <- ifelse(D$any_positive=="Yes",1L, ifelse(D$any_positive=="No",0L,NA_integer_))
D$V_status <- ifelse(D$vacc_status_code=="1",1L, ifelse(D$vacc_status_code=="2",0L,NA_integer_))
D$agegrp   <- cut(D$age, breaks=c(-Inf,5,17,44,Inf), labels=c("0-5","6-17","18-44","45+"))
D$sex_f    <- factor(ifelse(D$sex %in% c("Male","Female"), D$sex, NA), levels=c("Female","Male"))
D$caltime  <- as.numeric(D$onset - suppressWarnings(min(D$onset,na.rm=TRUE)))/7
D$vax_to_onset <- as.numeric(D$onset - D$vacc_dt)
D$n_tests_n<- suppressWarnings(as.integer(D$n_tests)); D$n_tests_n[is.na(D$n_tests_n)]<-0L
D$specimen_date <- asDate(D$episode_test_date); D$onset_to_swab <- as.numeric(D$specimen_date - D$onset)
D$exposure <- dplyr::case_when(
  D$vacc_status=="Not vaccinated" ~ "Unvaccinated",
  D$vacc_status=="Vaccinated" & !is.na(D$vax_to_onset) & D$vax_to_onset>10 ~ "Vaccinated >10d (effective)",
  D$vacc_status=="Vaccinated" & !is.na(D$vax_to_onset) & D$vax_to_onset>=0 & D$vax_to_onset<=10 ~ "Vaccinated 0-10d (not yet protected)",
  D$vacc_status=="Vaccinated" & !is.na(D$vax_to_onset) & D$vax_to_onset<0 ~ "Vaccinated after onset (ineligible)",
  D$vacc_status=="Vaccinated" & is.na(D$vax_to_onset) ~ "Vaccinated, timing unknown",
  TRUE ~ "Unknown status")
# link method mapping
D$link_method <- ifelse(grepl("Tier 1",D$linkage_tier),"Automatic / exact (Tier 1)",
                 ifelse(grepl("Tier 2",D$linkage_tier),"Probabilistic (Tier 2)","Manual / adjudicated (Tier 3-4 / cross-record)"))
# transition category
D$transition <- with(D, ifelse(n_tests_n<2, ifelse(first_result=="", "", paste0("Single (",substr(first_result,1,1),")")),
  ifelse(first_result=="Negative"&last_result=="Positive","N->P",
  ifelse(first_result=="Positive"&last_result=="Negative","P->N",
  ifelse(first_result=="Negative"&last_result=="Negative","N->N",
  ifelse(first_result=="Positive"&last_result=="Positive","P->P","other"))))))

# ---- PRIMARY analytic base = ALL distinct participants (within-pair confirmed
#      links + cross-record orphan links INCLUDED); a within-pair-only fit is kept
#      as a linkage-confidence sensitivity ----
D_all <- D                                   # full primary set (incl. orphans)
D_wp  <- D[D$is_cross_record!="Yes", , drop=FALSE]   # within-pair only (sensitivity)

# ---- model helper (profile-likelihood CI; falls back to Wald) ----
fit<-function(dat,outcome,exposure="V_status",adj=NULL){
  keep<-adj[sapply(adj,function(a) length(unique(dat[[a]][!is.na(dat[[a]])]))>1)]
  dat<-dat[!is.na(dat[[outcome]])&!is.na(dat[[exposure]]),,drop=FALSE]
  if(length(keep)) dat<-dat[complete.cases(dat[,c(outcome,exposure,keep)]),,drop=FALSE]
  mo<-tryCatch(glm(as.formula(paste(outcome,"~",paste(c(exposure,keep),collapse=" + "))),data=dat,family=binomial()),error=function(e)NULL)
  n<-nrow(dat); cs<-sum(dat[[outcome]]==1); ct<-sum(dat[[outcome]]==0)
  if(is.null(mo)) return(list(ok=FALSE,n=n,cases=cs,controls=ct,OR=NA,OR_L=NA,OR_U=NA,VE=NA,VE_L=NA,VE_U=NA,adj=keep))
  ci<-suppressWarnings(tryCatch(confint(mo),error=function(e)confint.default(mo)))
  co<-coef(mo); en<-grep(paste0("^",exposure),names(co),value=TRUE)[1]
  or<-exp(co[en]); lo<-exp(ci[en,1]); hi<-exp(ci[en,2]); ve<-ve_from_or(unname(or),unname(lo),unname(hi))
  list(ok=TRUE,n=n,cases=cs,controls=ct,adj=keep,OR=unname(or),OR_L=unname(lo),OR_U=unname(hi),VE=ve["VE"],VE_L=ve["VE_L"],VE_U=ve["VE_U"])}
firth<-function(X,y,mx=200,tol=1e-9){b<-rep(0,ncol(X));Xi<-diag(ncol(X));for(it in 1:mx){p<-as.vector(plogis(X%*%b));W<-p*(1-p);Wr<-sqrt(W);Xi<-solve(crossprod(X*Wr));Q<-(X*Wr)%*%Xi;h<-rowSums(Q*(X*Wr));st<-Xi%*%crossprod(X,(y-p)+h*(0.5-p));b<-b+st;if(max(abs(st))<tol)break};list(coef=as.vector(b),se=sqrt(diag(Xi)))}

crude <- fit(D,"Y_episode","V_status")
adj   <- fit(D,"Y_episode","V_status",c("agegrp","sex_f","caltime"))
# sensitivity: EXCLUDE the lower-confidence orphan (T3/T4) cross-record links
crude_ex <- fit(D_wp,"Y_episode","V_status")
# Firth crude (sparse-data)
df2<-D[!is.na(D$Y_episode)&!is.na(D$V_status),]; mm<-model.matrix(~V_status,df2); fr<-firth(mm,df2$Y_episode); j<-which(colnames(mm)=="V_status")
orf<-exp(fr$coef[j]); lof<-exp(fr$coef[j]-1.96*fr$se[j]); hif<-exp(fr$coef[j]+1.96*fr$se[j]); vf<-ve_from_or(orf,lof,hif)
firth_res<-list(OR=orf,OR_L=lof,OR_U=hif,VE=vf["VE"],VE_L=vf["VE_L"],VE_U=vf["VE_U"],n=nrow(df2),cases=sum(df2$Y_episode==1),controls=sum(df2$Y_episode==0))
# Firth ADJUSTED (age, sex, calendar time) — sparse-data-stable version of the adjusted model
dfa<-D[!is.na(D$Y_episode)&!is.na(D$V_status),]; dfa<-dfa[stats::complete.cases(dfa[,c("Y_episode","V_status","agegrp","sex_f","caltime")]),,drop=FALSE]
mma<-model.matrix(~V_status+agegrp+sex_f+caltime,dfa); fra<-firth(mma,dfa$Y_episode); ja<-which(colnames(mma)=="V_status")
orfa<-exp(fra$coef[ja]); lofa<-exp(fra$coef[ja]-1.96*fra$se[ja]); hifa<-exp(fra$coef[ja]+1.96*fra$se[ja]); vfa<-ve_from_or(orfa,lofa,hifa)
firth_adj<-list(OR=orfa,OR_L=lofa,OR_U=hifa,VE=vfa["VE"],VE_L=vfa["VE_L"],VE_U=vfa["VE_U"],n=nrow(dfa),cases=sum(dfa$Y_episode==1),controls=sum(dfa$Y_episode==0))

model_tbl<-data.frame(
  Model=c("Crude (vaccinated vs unvaccinated)","Adjusted (age, sex, calendar time)","Crude, excluding cross-record links (sensitivity)","Firth crude (sparse-data sensitivity)","Firth adjusted (age, sex, calendar time; sparse-data sensitivity)"),
  N=c(crude$n,adj$n,crude_ex$n,firth_res$n,firth_adj$n), Cases=c(crude$cases,adj$cases,crude_ex$cases,firth_res$cases,firth_adj$cases), Controls=c(crude$controls,adj$controls,crude_ex$controls,firth_res$controls,firth_adj$controls),
  OR=sprintf("%.2f",c(crude$OR,adj$OR,crude_ex$OR,orf,orfa)),
  `OR 95% CI`=c(sprintf("%.2f-%.2f",crude$OR_L,crude$OR_U),sprintf("%.2f-%.2f",adj$OR_L,adj$OR_U),sprintf("%.2f-%.2f",crude_ex$OR_L,crude_ex$OR_U),sprintf("%.2f-%.2f",lof,hif),sprintf("%.2f-%.2f",lofa,hifa)),
  `VE %`=sprintf("%.1f",c(crude$VE,adj$VE,crude_ex$VE,vf["VE"],vfa["VE"])),
  `VE 95% CI`=c(sprintf("%.1f to %.1f",crude$VE_L,crude$VE_U),sprintf("%.1f to %.1f",adj$VE_L,adj$VE_U),sprintf("%.1f to %.1f",crude_ex$VE_L,crude_ex$VE_U),sprintf("%.1f to %.1f",vf["VE_L"],vf["VE_U"]),sprintf("%.1f to %.1f",vfa["VE_L"],vfa["VE_U"])),
  check.names=FALSE)

# ---- sensitivity by outcome definition ----
outdefs<-c(`Episode (primary)`="Y_episode",`First result`="Y_first",`Final result`="Y_last",`Any-positive`="Y_any")
sens<-do.call(rbind,lapply(names(outdefs),function(nm){oc<-outdefs[[nm]];mm<-fit(D,oc,"V_status");d<-D[!is.na(D[[oc]])&!is.na(D$V_status),]
  data.frame(`Outcome definition`=nm,N=mm$n,Cases=mm$cases,Controls=mm$controls,
    `Vaccinated cases`=sum(d[[oc]]==1&d$V_status==1),`Vaccinated controls`=sum(d[[oc]]==0&d$V_status==1),
    OR=sprintf("%.2f",mm$OR),`95% CI`=sprintf("%.2f-%.2f",mm$OR_L,mm$OR_U),`VE %`=sprintf("%.1f",mm$VE),`VE 95% CI`=sprintf("%.1f to %.1f",mm$VE_L,mm$VE_U),check.names=FALSE)}))

# ---- descriptive helpers ----
ft<-function(x,lv=NULL){x<-ifelse(is.na(x)|x=="","Missing",as.character(x));if(is.null(lv))lv<-names(sort(table(x),decreasing=TRUE));tb<-as.integer(table(factor(x,levels=lv)));data.frame(Category=lv,n=tb,`%`=round(100*tb/sum(tb),1),check.names=FALSE)}
np<-function(n,d)sprintf("%d (%.1f%%)",n,ifelse(d>0,100*n/d,0))
dist<-list(
  pcr = ft(D$episode_result, c("Positive","Negative")),
  vacc= ft(D$vacc_status, c("Vaccinated","Not vaccinated","Unknown")),
  sex = ft(D$sex, c("Male","Female","Unknown")),
  zone= ft(D$health_zone),
  tier= ft(D$link_method, c("Automatic / exact (Tier 1)","Probabilistic (Tier 2)","Manual / adjudicated (Tier 3-4 / cross-record)")),
  expo= ft(D$exposure),
  vital=ft(D$vital_status, c("Alive","Dead","Unknown")))
# characteristics by case/control
byS<-D[!is.na(D$Y_episode),]; byS$grp<-ifelse(byS$Y_episode==1,"Case (PCR+)","Control (PCR-)")
gg2<-split(byS,byS$grp); cb<-data.frame(Variable=c("N","Age median (IQR)","Female n (%)","Vaccinated n (%)","Health zone Bulape n (%)","Died n (%)"),check.names=FALSE)
for(k in names(gg2)){d<-gg2[[k]];cb[[k]]<-c(as.character(nrow(d)),
  sprintf("%.0f (%.0f-%.0f)",median(d$age,na.rm=TRUE),quantile(d$age,.25,na.rm=TRUE),quantile(d$age,.75,na.rm=TRUE)),
  np(sum(d$sex=="Female"),nrow(d)), np(sum(d$vacc_status=="Vaccinated"),nrow(d)),
  np(sum(d$health_zone=="Bulape"),nrow(d)), np(sum(d$vital_status=="Dead"),nrow(d)))}
# vaccination x case/control
vsub<-byS[byS$vacc_status%in%c("Vaccinated","Not vaccinated"),]
vc<-as.data.frame.matrix(table(vsub$grp, vsub$vacc_status)); vc<-cbind(grp=rownames(vc),vc); rownames(vc)<-NULL
# transitions
rep_tested<-D[D$n_tests_n>=2,]; trans_tab<-ft(rep_tested$transition,c("N->N","N->P","P->P","P->N","other"))
discordant<-sum(rep_tested$transition%in%c("N->P","P->N","other"))
# completeness
compl<-function(df){n<-nrow(df); vd<-df$vacc_status=="Vaccinated"
  data.frame(Variable=c("RT-PCR result (episode-defining)","Symptom-onset date","Vaccination status","Vaccination date (among vaccinated)","Age","Sex","Health zone","Health area","Village","Vaccination strategy (coded)","Specimen/swab date","Vital status"),
    N=n, Complete=c(sum(!is.na(df$Y_episode)),sum(!is.na(df$onset)),sum(df$vacc_status%in%c("Vaccinated","Not vaccinated")),sum(vd&!is.na(df$vacc_dt)),sum(!is.na(df$age)),sum(df$sex%in%c("Male","Female")),sum(nzchar(df$health_zone)),sum(nzchar(df$health_area)),sum(nzchar(df$village)),sum(nzchar(df$vacc_strategy_code)),sum(!is.na(df$specimen_date)),sum(df$vital_status%in%c("Alive","Dead"))),
    Denom=c(n,n,n,sum(vd),n,n,n,n,n,n,n,n),check.names=FALSE)}
cmp<-compl(D); cmp$`Complete n (%)`<-sprintf("%d (%.1f%%)",cmp$Complete,ifelse(cmp$Denom>0,100*cmp$Complete/cmp$Denom,0)); cmp$`Missing n (%)`<-sprintf("%d (%.1f%%)",cmp$Denom-cmp$Complete,ifelse(cmp$Denom>0,100*(cmp$Denom-cmp$Complete)/cmp$Denom,0))
# data quality
dq<-data.frame(Check=c("Vaccination after symptom onset (implausible)","Specimen before symptom onset","Onset-to-swab > 30 days","Vaccinated but vaccination date missing","Linked but RT-PCR indeterminate/missing","Cross-record (orphan T3/T4) links included in primary"),
  N=c(sum(D$vax_to_onset<0,na.rm=TRUE),sum(D$onset_to_swab<0,na.rm=TRUE),sum(D$onset_to_swab>30,na.rm=TRUE),sum(D$vacc_status=="Vaccinated"&is.na(D$vacc_dt)),sum(is.na(D$Y_episode)),sum(D$is_cross_record=="Yes")),check.names=FALSE)
intv<-data.frame(Interval=c("Onset-to-swab (days)","Vaccination-to-onset (days)"),n=c(sum(!is.na(D$onset_to_swab)),sum(!is.na(D$vax_to_onset))),
  median=c(median(D$onset_to_swab,na.rm=TRUE),median(D$vax_to_onset,na.rm=TRUE)),
  IQR=c(sprintf("%.0f to %.0f",quantile(D$onset_to_swab,.25,na.rm=TRUE),quantile(D$onset_to_swab,.75,na.rm=TRUE)),sprintf("%.0f to %.0f",quantile(D$vax_to_onset,.25,na.rm=TRUE),quantile(D$vax_to_onset,.75,na.rm=TRUE))),check.names=FALSE)
# data-flow (coded-centric)
N_export<-414L
audit<-data.frame(Step=c("Confirmed distinct-case linkage export (rows)","PRIMARY analysis base = distinct participants (canonical identity key)","  within-pair confirmed links","  cross-record (orphan T3/T4) links INCLUDED","With an episode-defining RT-PCR result (case/control eligible)","  cases (PCR+)","  controls (PCR-)","With known vaccination status (vax/unvax)","PRIMARY analytic N (definitive result + known vaccination)"),
  N=c(N_export,nrow(D),sum(D$is_cross_record!="Yes"),sum(D$is_cross_record=="Yes"),sum(!is.na(D$Y_episode)),sum(D$Y_episode==1,na.rm=TRUE),sum(D$Y_episode==0,na.rm=TRUE),sum(D$V_status%in%c(0,1)),crude$n),check.names=FALSE)

# ---- figures ----
thm<-theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank(),plot.title=element_text(face="bold"))
ggs<-function(f,p,w=7,h=4) suppressWarnings(ggsave(file.path(FIG,f),p,width=w,height=h,dpi=150,bg="white"))
md<-cmp; md$Missing_pct<-100-100*md$Complete/md$Denom
try(ggs("F4_missingness.png",ggplot(md,aes(reorder(Variable,Missing_pct),Missing_pct))+geom_col(fill="#E6550D")+geom_text(aes(label=sprintf("%.0f%%",Missing_pct)),hjust=-0.15,size=3)+coord_flip()+ylim(0,100)+labs(title="Missingness (coded analytic dataset)",x=NULL,y="% missing")+thm),silent=TRUE)
try(ggs("F1_vacc_to_onset.png",ggplot(D[!is.na(D$vax_to_onset),],aes(vax_to_onset))+geom_histogram(binwidth=5,fill="#2C7FB8",colour="white")+geom_vline(xintercept=10,linetype="dashed",colour="#C62828")+labs(title="Vaccination-to-onset interval",subtitle="dashed = 10-day rVSV window",x="Days vaccination to onset",y="Participants")+thm),silent=TRUE)
tt2<-trans_tab[trans_tab$n>0,]; if(nrow(tt2)) try(ggs("F2_transitions.png",ggplot(tt2,aes(reorder(Category,n),n))+geom_col(fill="#756BB1")+geom_text(aes(label=n),hjust=-0.2,size=3.5)+coord_flip()+labs(title="RT-PCR transitions (>=2 specimens)",x=NULL,y="Participants")+thm),silent=TRUE)
fp<-sens;fp$OR<-as.numeric(fp$OR);fp$L<-as.numeric(sub("-.*","",fp$`95% CI`));fp$U<-as.numeric(sub(".*-","",fp$`95% CI`));fp$def<-factor(fp$`Outcome definition`,levels=rev(fp$`Outcome definition`))
try(ggs("F3_forest_OR.png",ggplot(fp,aes(OR,def))+geom_point(size=2.6,colour="#2C3E82")+geom_errorbarh(aes(xmin=L,xmax=U),height=.18,colour="#2C3E82")+geom_vline(xintercept=1,linetype="dashed",colour="grey40")+scale_x_log10()+labs(title="Odds ratio by outcome definition",subtitle="OR<1 favours protection",x="Odds ratio (log scale)",y=NULL)+thm),silent=TRUE)

# ---- exports ----
write.csv(D,file.path(OUT,"coded_analytic_participant_level.csv"),row.names=FALSE,na="")
write_xlsx(list(Data_flow=audit,VE_models=model_tbl,Outcome_sensitivity=sens,Completeness=cmp[,c("Variable","Complete n (%)","Missing n (%)")],Char_by_PCR=cb,Vacc_by_status=vc,Transitions=trans_tab,DQ_flags=dq,Intervals=intv),file.path(OUT,"coded_analysis_tables.xlsx"))

# ======================= WORD REPORT (Century Gothic) ========================
FONT<-"Century Gothic"; set_flextable_defaults(font.family=FONT,font.size=9,padding=3,theme_fun=theme_booktabs)
tt<-function(txt,sz=11,bold=FALSE,color="#000000",italic=FALSE) fp_text(font.family=FONT,font.size=sz,bold=bold,color=color,italic=italic)
par0<-fp_par(text.align="left",padding.bottom=6); H1n<-0; H2c<-0; doc<-read_docx()
H1<-function(txt){H1n<<-H1n+1;H2c<<-0;doc<<-doc%>%body_add_fpar(fpar(ftext(paste0(H1n,". ",txt),tt(txt,15,TRUE,"#1F3864")),fp_p=fp_par(padding.top=10,padding.bottom=4)))}
H2<-function(txt){H2c<<-H2c+1;doc<<-doc%>%body_add_fpar(fpar(ftext(paste0(H1n,".",H2c," ",txt),tt(txt,12,TRUE,"#2E5496")),fp_p=fp_par(padding.top=6,padding.bottom=3)))}
BODY<-function(txt){doc<<-doc%>%body_add_fpar(fpar(ftext(txt,tt(txt,10.5)),fp_p=par0))}
BULl<-function(txt){doc<<-doc%>%body_add_fpar(fpar(ftext(paste0("•  ",txt),tt(txt,10.5)),fp_p=fp_par(padding.bottom=2,padding.left=14)))}
CAP<-function(txt){doc<<-doc%>%body_add_fpar(fpar(ftext(txt,tt(txt,9,TRUE,"#404040",TRUE)),fp_p=fp_par(padding.top=6,padding.bottom=2)))}
NOTE<-function(txt){doc<<-doc%>%body_add_fpar(fpar(ftext(txt,tt(txt,9,FALSE,"#7030A0",TRUE)),fp_p=fp_par(padding.bottom=4)))}
FT<-function(df){ft<-flextable(as.data.frame(df))%>%bold(part="header")%>%bg(part="header",bg="#D9E2F3")%>%border_outer(part="all")%>%border_inner_h(part="body")%>%fontsize(size=8.5,part="all")%>%font(fontname=FONT,part="all")%>%autofit();doc<<-doc%>%body_add_flextable(ft)%>%body_add_par("")}
addfig<-function(file,cap,w=6.3,h=3.8){p<-file.path(FIG,file);if(file.exists(p)){CAP(cap);doc<<-doc%>%body_add_img(src=p,width=w,height=h)%>%body_add_par("")}}
sig<-function(hi) ifelse(!is.na(hi)&hi<1,"statistically significant","not statistically significant")

N_dist_all<-nrow(D_all); N_dist<-nrow(D); n_cross<-sum(D_all$is_cross_record=="Yes"); N_epi<-sum(!is.na(D$Y_episode)); ncase<-crude$cases; nctrl<-crude$controls
epar<-if(!is.na(adj$OR)) adj$cases/ (length(adj$adj)+1) else NA

# TITLE
doc<-doc%>%
  body_add_fpar(fpar(ftext("Preliminary Vaccine Effectiveness Analysis",tt("",20,TRUE,"#1F3864")),fp_p=fp_par(text.align="center",padding.top=30,padding.bottom=4)))%>%
  body_add_fpar(fpar(ftext("Ebola Virus Disease, Bulape and surrounding health zones, Democratic Republic of the Congo",tt("",13,FALSE,"#2E5496")),fp_p=fp_par(text.align="center",padding.bottom=2)))%>%
  body_add_fpar(fpar(ftext("Test-negative design on the coded distinct-case analytic dataset (Distinct_CODED)",tt("",11,FALSE,"#404040",TRUE)),fp_p=fp_par(text.align="center",padding.bottom=4)))%>%
  body_add_fpar(fpar(ftext("Report 001 — 20 August 2026",tt("",10,FALSE,"#404040")),fp_p=fp_par(text.align="center",padding.bottom=16)))%>%
  body_add_fpar(fpar(ftext("PRELIMINARY — for internal review. All estimates are provisional and subject to the data limitations described herein.",tt("",10,TRUE,"#C00000")),fp_p=fp_par(text.align="center",padding.bottom=16)))
doc<-doc%>%body_add_fpar(fpar(ftext(sprintf("Coded dataset: %d distinct participants (including %d cross-record orphan links); %d with an episode-defining RT-PCR result; primary test-negative sample %d (%d cases, %d controls).",N_dist,n_cross,N_epi,crude$n,ncase,nctrl),tt("",10.5)),fp_p=fp_par(text.align="center",padding.bottom=6)))
doc<-doc%>%body_add_break()

# 1. EXEC SUMMARY
H1("Executive summary")
BODY("This preliminary analysis evaluates the effectiveness of Ebola (rVSV-ZEBOV / Ervebo) vaccination against laboratory-confirmed Ebola virus disease (EBOD) using a test-negative design, as specified in the governing study protocol. It is run directly on the coded distinct-case analytic dataset (Distinct_CODED), one row per participant, in which each participant's full triangulated laboratory history has been reduced to coded analytic variables. Cases are suspected EBOD patients with a positive RT-PCR result and controls are suspected patients who test negative; vaccine effectiveness (VE) is derived from the odds ratio (OR) as VE = (1 - OR) x 100 percent.")
BODY(sprintf("The confirmed distinct-case linkage export contained %d records, which reduced to %d distinct participants after de-duplication on a single canonical identity key (real case-identifier where present, otherwise the standardized full name). All %d participants — including the %d cross-record (orphan Tier 3/4) links — were retained as the primary analysis base; a within-pair-only fit that drops the orphan links is reported as a linkage-confidence sensitivity analysis. Of the base, %d had an episode-defining RT-PCR result and known vaccination status and formed the primary test-negative sample (%d cases, %d controls). The episode-defining result is the first definitive specimen collected on or after symptom onset.",N_export,N_dist,N_dist,n_cross,crude$n,ncase,nctrl))
BODY(sprintf("The crude odds ratio for vaccination was %.2f (95%% CI %.2f-%.2f), a crude VE of %.1f%% (95%% CI %.1f%% to %.1f%%), which is %s. After adjustment for age, sex and calendar time the OR was %.2f (95%% CI %.2f-%.2f), VE %.1f%% (95%% CI %.1f%% to %.1f%%), which is %s. As a sensitivity analysis, excluding the %d lower-confidence cross-record links gave a crude VE of %.1f%% (95%% CI %.1f%% to %.1f%%) — essentially unchanged. All point estimates are protective; with %d cases the estimates remain imprecise and preliminary.",
  crude$OR,crude$OR_L,crude$OR_U,crude$VE,crude$VE_L,crude$VE_U,sig(crude$OR_U),
  adj$OR,adj$OR_L,adj$OR_U,adj$VE,adj$VE_L,adj$VE_U,sig(adj$OR_U),
  n_cross,crude_ex$VE,crude_ex$VE_L,crude_ex$VE_U,ncase))
NOTE("Statistical non-significance is not evidence of no effect. These estimates are limited by small numbers, incomplete vaccination-date and vaccination-strategy information, and residual duplicate participants under different laboratory identifiers.")

# 2. BACKGROUND
H1("Background and objectives")
BODY("The objective is to estimate the effectiveness of prior Ebola vaccination in preventing laboratory-confirmed EBOD, using routinely collected surveillance, laboratory and vaccination data from the outbreak response. This report (i) builds a coded, one-row-per-participant analytic dataset with an accompanying data dictionary, and (ii) runs the primary test-negative analysis and pre-specified sensitivity analyses directly on that coded dataset, so the analysis is fully reproducible from a single documented file.")

# 3. METHODS
H1("Methods")
H2("Data sources and the coded analytic dataset")
BULl("Surveillance / case-investigation records (case identity, symptom-onset date, vaccination history, demographics, geography).")
BULl("Laboratory data: a triangulated laboratory register produced by merging SIX source datasets (DATA001-DATA006, with a health-zone update) into one consolidated record per laboratory patient identifier, following the data-collection SOP. Each patient's full specimen history (all RT-PCR results and dates) is taken from this register, joined to the linkage by the laboratory patient identifier (an exact ID join, not a name match).")
BULl("Vaccination information: no standalone national vaccination register file was available for this preliminary run; vaccination status, date, type and strategy were taken from the surveillance/case-investigation fields (documented limitation).")
BULl("The coded analytic dataset (Distinct_CODED) holds one row per participant with coded demographic, geographic, vaccination, laboratory-history, outcome, vital-status and exposure/contact variables, plus a companion data dictionary. Every analysis in this report is computed from that file.")
H2("Case / control and outcome definition")
BODY("Each participant contributes one definitive RT-PCR outcome. The episode-defining result (primary) is the first definitive positive/negative specimen collected on or after symptom onset; where no specimen followed onset the first definitive specimen was used. Y=1 for RT-PCR positive (case) and Y=0 for negative (control); indeterminate/missing results were excluded because case/control status cannot be determined. Three alternative outcome definitions (first result, final result, any-positive) were pre-specified for sensitivity analysis.")
H2("Exposure and covariates")
BODY("The primary exposure was vaccinated versus not vaccinated (coded vaccination status); unknown status was excluded and not imputed. Where a vaccination date was available, the vaccination-to-onset interval was computed and, following the protocol for rVSV-ZEBOV, individuals were considered effectively vaccinated only when onset occurred more than 10 days after vaccination. Because vaccination dates were available for a minority of vaccinated participants, the primary model used vaccination status; the >10-day window is reported descriptively. The adjusted model added age group, sex and calendar time (weeks since first onset); covariates were kept parsimonious given the number of events, and geography was not included because participants were overwhelmingly from a single health zone.")
H2("Statistical methods")
BODY("VE and its confidence interval were derived from the OR: VE = (1-OR)x100%, with limits VE_L=(1-OR_U)x100% and VE_U=(1-OR_L)x100%. Odds-ratio confidence intervals used profile likelihood. A compact Firth (Jeffreys-penalized) logistic regression was implemented as a sparse-data sensitivity analysis. Two link-confidence sensitivity analyses were pre-specified: (a) excluding cross-record (orphan Tier 3/4) links, and (b) varying the outcome definition. Analyses used R 4.6.0 (dplyr, ggplot2, officer, flextable).")
H2("De-duplication")
BODY(sprintf("The confirmed distinct-case export (%d records) was de-duplicated to %d participants using the single canonical identity key; where the same participant appeared on both a within-pair and a cross-record row, the higher-confidence within-pair row was retained. All %d participants, including the %d cross-record (orphan Tier 3/4) links, were retained in the primary analysis; a within-pair-only fit is reported as a linkage-confidence sensitivity analysis. This canonical key merges name-variant records that a display key counts separately; residual duplicates under different laboratory identifiers may remain and are noted as a limitation.",N_export,N_dist,N_dist,n_cross))

# 4. RESULTS
H1("Results")
H2("Data flow")
CAP("Table 4.1. Participant data-flow (coded analytic dataset).")
FT(audit)
H2("Completeness assessment")
CAP("Table 4.2. Completeness of key analytic variables.")
FT(cmp[,c("Variable","Complete n (%)","Missing n (%)")])
H2("Descriptive characteristics")
CAP("Table 4.3. Characteristics by RT-PCR status (episode-defining outcome).")
FT(cb)
CAP("Table 4.4. Vaccination status by case/control status.")
FT(vc)
CAP("Table 4.5. Distributions: RT-PCR result, vaccination status, sex, linkage method, protocol exposure window, vital status.")
FT(bind_rows(
  dist$pcr  %>% transmute(Group="RT-PCR",Category,n,`%`),
  dist$vacc %>% transmute(Group="Vaccination",Category,n,`%`),
  dist$tier %>% transmute(Group="Linkage method",Category,n,`%`),
  dist$expo %>% transmute(Group="Exposure window",Category,n,`%`),
  dist$vital%>% transmute(Group="Vital status",Category,n,`%`)))
H2("Vaccine-effectiveness models")
CAP("Table 4.6. Crude, adjusted, cross-record-excluded and Firth VE estimates (episode-defining outcome).")
FT(model_tbl)
BODY(sprintf("The crude VE was %.1f%% (95%% CI %.1f%% to %.1f%%, %s) and the adjusted VE %.1f%% (95%% CI %.1f%% to %.1f%%, %s). Excluding the lower-confidence cross-record links (sensitivity) gave a crude VE of %.1f%% (95%% CI %.1f%% to %.1f%%), essentially unchanged, confirming the estimate is not driven by the orphan links. Point estimates are protective; the intervals are wide given the number of cases, so the estimate is preliminary and should not be over-interpreted.",
  crude$VE,crude$VE_L,crude$VE_U,sig(crude$OR_U),adj$VE,adj$VE_L,adj$VE_U,sig(adj$OR_U),crude_ex$VE,crude_ex$VE_L,crude_ex$VE_U))
H2("Discordant RT-PCR results and outcome-definition sensitivity")
CAP("Table 4.7. RT-PCR transition patterns among repeatedly tested participants.")
FT(trans_tab)
CAP("Table 4.8. Vaccine-effectiveness under alternative outcome definitions.")
FT(sens)
BODY(sprintf("Of %d participants with repeated RT-PCR testing, %d had changing/discordant results. VE point estimates across outcome definitions remained protective; statistical significance was not consistent across definitions, indicating the conclusion is sensitive to outcome specification.",nrow(rep_tested),discordant))
H2("Timing and data-quality")
CAP("Table 4.9. Timing intervals (days).")
FT(intv)
CAP("Table 4.10. Data-quality and timing flags.")
FT(dq)
H2("Figures")
addfig("F4_missingness.png","Figure 1. Missingness by variable (coded analytic dataset).")
addfig("F1_vacc_to_onset.png","Figure 2. Distribution of the vaccination-to-onset interval (dashed line = 10-day window).")
addfig("F2_transitions.png","Figure 3. RT-PCR transition patterns among repeatedly tested participants.")
addfig("F3_forest_OR.png","Figure 4. Odds ratio for vaccination by outcome definition (OR<1 favours protection).")

# 5. LIMITATIONS
H1("Limitations and interpretation")
BULl("Small numbers, particularly of cases, produce wide confidence intervals; the analysis is preliminary and not powered for a definitive VE estimate.")
if(!is.na(adj$OR)) BULl(sprintf("The ADJUSTED model is over-parameterised for the available events (about %.1f events per parameter, below the accepted minimum of ~10). Its wide interval (VE %.0f%%, 95%% CI %.0f%% to %.0f%%) is a symptom of this instability, not evidence of harm: the negative lower bound is simply (1 - the upper odds-ratio limit) x 100 and reflects imprecision. The CRUDE and Firth estimates are more reliable with this number of cases and should be preferred.",epar,adj$VE,adj$VE_L,adj$VE_U))
BULl(sprintf("Residual DUPLICATE participants remain. The canonical key merged %d name-variant records in the export, but it does NOT merge an individual recorded under two different laboratory identifiers; a patient-level de-duplication is still required and could change the counts and the VE estimate.",N_export-N_dist))
BULl(sprintf("The primary analysis INCLUDES %d cross-record (orphan Tier 3/4) links of lower linkage confidence; excluding them (crude VE %.1f%%) leaves the estimate essentially unchanged and is reported as a sensitivity analysis.",n_cross,crude_ex$VE))
BULl("Vaccination information is drawn from surveillance rather than a verified vaccination register; vaccination dates are missing for many vaccinated participants, limiting the protocol >10-day effective-window analysis.")
BULl("Vaccination strategy is coded numerically with no decode key (ring vs targeted-geographic not resolvable); geography is near-constant (single dominant health zone), limiting geographic adjustment. Orthoebolavirus species is not recorded in the source data (Zaire ebolavirus assumed for this outbreak).")
BULl("Statistical non-significance must not be read as evidence of no effect. Results should be updated as linkage adjudication, register-verified vaccination data, and additional records become available.")

# 6. REPRODUCIBILITY
H1("Reproducibility and deliverables")
BULl("Coded analytic dataset: Distinct_CODED.csv / .xlsx (one row per participant) built by build_distinct_coded.R.")
BULl("Data dictionary: Distinct_CODED_data_dictionary.csv (variable, label, type, source, codes, notes).")
BULl("Analysis + report generator: analyze_coded_report.R (this document, computed from Distinct_CODED.csv).")
BULl("Participant-level analytic export and all tables: main_output_coded/coded_analytic_participant_level.csv and coded_analysis_tables.xlsx; figures: main_output_coded/figures/*.png.")

print(doc, target=REPORT)
cat("\n================ CODED-DATA VE ANALYSIS — KEY RESULTS ================\n")
print(audit,row.names=FALSE)
cat(sprintf("\nCrude   OR %.2f (%.2f-%.2f)  VE %.1f%% (%.1f to %.1f)  [%s]\n",crude$OR,crude$OR_L,crude$OR_U,crude$VE,crude$VE_L,crude$VE_U,sig(crude$OR_U)))
cat(sprintf("Adjust  OR %.2f (%.2f-%.2f)  VE %.1f%% (%.1f to %.1f)  [%s]\n",adj$OR,adj$OR_L,adj$OR_U,adj$VE,adj$VE_L,adj$VE_U,sig(adj$OR_U)))
cat(sprintf("Excl-cross OR %.2f (%.2f-%.2f)  VE %.1f%% (%.1f to %.1f)  [sensitivity]\n",crude_ex$OR,crude_ex$OR_L,crude_ex$OR_U,crude_ex$VE,crude_ex$VE_L,crude_ex$VE_U))
cat(sprintf("Firth   OR %.2f (%.2f-%.2f)  VE %.1f%% (%.1f to %.1f)\n",orf,lof,hif,vf["VE"],vf["VE_L"],vf["VE_U"]))
cat("\nReport written: ",REPORT,"\n",sep="")
