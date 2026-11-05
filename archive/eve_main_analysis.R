# =============================================================================
#  eve_main_analysis.R  —  SINGLE consolidated script for the confirmed-linkage
#  (MAIN) preliminary VE analysis, with all available lab samples per patient.
#
#  ONE canonical participant key is used everywhere:
#     pkey = real case-id if present, else normalized full name (name_4 + name_5)
#  A built-in ACCOUNTING table reconciles every count so numbers cannot drift.
#
#  Supersedes main_broad_analysis.R.  Nothing hard-coded; all values computed.
#  Outputs -> EVE_VE_Analysis/main_output/  (dataset, accounting, VE tables, bundle, figures)
# =============================================================================
suppressWarnings(suppressMessages({library(dplyr); library(readxl); library(writexl); library(ggplot2)}))
STD <- "C:/Users/hp/Documents/data/DashShiny_EVE_BackUp_Shared/data"
LABX<- file.path(STD,"archive","lab_archive","General_Base_LABO_MVE_Bulape_21NOV2025_EN.xlsx")
OUT <- "C:/Users/hp/Documents/data/EVE_VE_Analysis/main_output"; dir.create(OUT,showWarnings=FALSE,recursive=TRUE)
FIG <- file.path(OUT,"figures"); dir.create(FIG,showWarnings=FALSE)

rc<-function(p) read.csv(p,colClasses="character",check.names=FALSE,na.strings=character(0),encoding="UTF-8")
blank<-function(x){if(is.null(x))return(character(0));x[is.na(x)]<-"";trimws(x)}
norm<-function(s){s<-blank(s);t<-suppressWarnings(iconv(s,to="ASCII//TRANSLIT"));s<-ifelse(is.na(t),s,t);toupper(gsub("\\s+"," ",trimws(s)))}
numv<-function(x){suppressWarnings(as.numeric(gsub("[^0-9.\\-]","",blank(x))))}
isRealId<-function(id){id<-toupper(blank(id));nzchar(id)&grepl("[0-9]",id)&!grepl("^(RAS|INCONNU|RDC|NA)$",id)}
pkey<-function(n1,n4,n5) ifelse(isRealId(n1),paste0("ID#",norm(n1)),paste0("NM#",norm(paste0(n4," ",n5))))
pdate<-function(x){x<-trimws(blank(x));out<-as.Date(rep(NA_real_,length(x)),origin="1970-01-01")
  for(f in c("%Y-%m-%d","%d-%b-%Y","%d/%m/%Y","%d-%m-%Y","%d.%m.%Y")){mm<-is.na(out)&nzchar(x);if(!any(mm))break;out[mm]<-suppressWarnings(as.Date(x[mm],format=f))}
  ser<-is.na(out)&grepl("^[0-9]{5}$",x); if(any(ser)) out[ser]<-as.Date(as.numeric(x[ser]),origin="1899-12-30"); out}
ve_from_or<-function(or,lo,hi){or<-unname(or);lo<-unname(lo);hi<-unname(hi);c(VE=(1-or)*100,VE_L=(1-hi)*100,VE_U=(1-lo)*100)}
MAP_VACC<-c("1"="Vaccinated","2"="Not vaccinated","99"="Unknown"); MAP_SEX<-c("1"="Male","2"="Female","3"="Unknown")

# ---- 1. load ----
m   <- rc(file.path(STD,"linked_data","linked_dataset_MAIN.csv"))
sv  <- rc(file.path(STD,"raw_data","ebola_vaccine_effectiveness_surveillance.csv"))
merg<- rc(file.path(STD,"raw_data","merged_eve_lab_data.csv")); uid2name<-setNames(blank(merg$patient_name_clean),blank(merg$patient_uid))
# AUTHORITATIVE lab sample histories: the triangulation WIDE file, joined by
# patient_uid (== MAIN lab_sample_id). This is an ID join, not a name match, so
# it avoids the name-collision errors that previously mis-assigned samples.
WIDE<-"C:/Users/hp/Desktop/lab_data_triangulation/outputs/merged_eve_lab_wide.csv"
W<-rc(WIDE); Wuid<-blank(W$patient_uid)
resCols<-grep("^final_result_evd_[0-9]+$",names(W),value=TRUE); resN<-as.integer(sub(".*_","",resCols))
resCols<-resCols[order(resN)]; resN<-sort(resN)
g<-suppressWarnings(as.data.frame(read_excel(LABX,sheet="Global",col_types="text")))  # kept only for the sample-register row count in the accounting

# ---- 2. dedup MAIN to one row per participant (CANONICAL pkey) ----
m$pk   <- pkey(m$name_1,m$name_4,m$name_5)
m$prio <- ifelse(blank(m$final_status)=="Reviewer-accepted",0,1)
m$tiern<- suppressWarnings(as.integer(sub(".*Tier\\s*([0-9]).*","\\1",m$linkage_tier)))
mo <- m[order(m$pk,m$prio,m$tiern),]; dupN<-ave(rep(1L,nrow(mo)),mo$pk,FUN=length)
mb <- mo[!duplicated(mo$pk),]; mb$main_dup_n<-as.integer(dupN[!duplicated(mo$pk)])
N_MAIN_ROWS<-nrow(m); N_PART<-nrow(mb)

# ---- 3. lab histories: ALL Global samples per participant; else single merged result ----
mb$lab_uid <- blank(mb$lab_sample_id); mb$lab_name<-uid2name[mb$lab_uid]; mb$lab_name[is.na(mb$lab_name)]<-""
mb$onset   <- pdate(mb$date_frist_sympt)
hist_of<-function(uid,onset){ wi<-match(uid, Wuid); if(is.na(wi)) return(NULL)
  res<-blank(unlist(W[wi, resCols]))
  dcol<-paste0("test_result_date_",resN); dv<-vapply(seq_along(dcol),function(k) if(dcol[k]%in%names(W)) blank(W[[dcol[k]]][wi]) else "", character(1))
  dt<-pdate(dv)
  keep<-res %in% c("Positive","Negative"); if(!any(keep)) return(NULL)
  res<-res[keep]; dt<-dt[keep]
  o<-order(is.na(dt), dt); res<-res[o]; dt<-dt[o]        # dated samples chronological; undated last
  epi<-if(!is.na(onset)) which(!is.na(dt) & dt>=onset)[1] else NA; if(is.na(epi)) epi<-1L
  list(n=length(res), first=res[1], final=res[length(res)], episode=res[epi], any_pos=any(res=="Positive"),
       first_dt=dt[1], epi_dt=dt[epi]) }
P<-data.frame(participant_key=mb$pk, case_id=blank(mb$name_1), surname=blank(mb$name_4), firstname=blank(mb$name_5),
  lab_uid=mb$lab_uid, lab_name=mb$lab_name, sex=unname(MAP_SEX[blank(mb$sex)]), age=numv(mb$age_years),
  health_zone=blank(mb$health_zone_1), health_area=blank(mb$health_area_1), village=blank(mb$village_residence),
  onset=mb$onset, vacc_status=unname(MAP_VACC[blank(mb$vaccin_mve)]), vacc_date=pdate(mb$date_vaccination1),
  vacc_type=blank(mb$type_vaccin1), vacc_strategy=blank(mb$vaccine_strategy),
  linkage_tier=blank(mb$linkage_tier), final_status=blank(mb$final_status), main_dup_n=mb$main_dup_n,
  evd_single=blank(mb$evd_result_final), stringsAsFactors=FALSE)
P$vacc_status[is.na(P$vacc_status)|P$vacc_status==""]<-"Unknown"
for(c in c("n_global_samples","first","final","episode","any_pos","lab_source")) P[[c]]<-NA
P$first_dt<-as.Date(NA); P$epi_dt<-as.Date(NA); P$n_global_samples<-0L
for(i in seq_len(nrow(P))){
  h<-hist_of(P$lab_uid[i],P$onset[i])
  if(!is.null(h)){ P$n_global_samples[i]<-h$n; P$first[i]<-h$first; P$final[i]<-h$final; P$episode[i]<-h$episode
    P$any_pos[i]<-h$any_pos; P$first_dt[i]<-h$first_dt; P$epi_dt[i]<-h$epi_dt; P$lab_source[i]<-"Lab register (all samples, uid-keyed)"
  } else if(P$evd_single[i] %in% c("Positive","Negative")){
    P$first[i]<-P$evd_single[i]; P$final[i]<-P$evd_single[i]; P$episode[i]<-P$evd_single[i]
    P$any_pos[i]<-P$evd_single[i]=="Positive"; P$lab_source[i]<-"Merged (single result)"
  } else P$lab_source[i]<-"No definitive result"
}
yf<-function(r) ifelse(r=="Positive",1L,ifelse(r=="Negative",0L,NA_integer_))
P$Y_episode<-yf(P$episode); P$Y_first<-yf(P$first); P$Y_final<-yf(P$final)
P$Y_any<-ifelse(is.na(P$any_pos),NA_integer_,ifelse(P$any_pos,1L,ifelse(P$episode=="Negative",0L,NA_integer_)))

# ---- 4. exposure + covariates ----
P$vax_to_onset<-as.numeric(P$onset-P$vacc_date)
P$V_status<-dplyr::case_when(P$vacc_status=="Vaccinated"~1L,P$vacc_status=="Not vaccinated"~0L,TRUE~NA_integer_)
P$agegrp<-cut(P$age,breaks=c(-Inf,5,17,44,Inf),labels=c("0-5","6-17","18-44","45+"))
P$sex_f<-factor(ifelse(P$sex%in%c("Male","Female"),P$sex,NA),levels=c("Female","Male"))
P$caltime<-as.numeric(P$onset-min(P$onset,na.rm=TRUE))/7
write.csv(P,file.path(OUT,"main_analytic_participant_level.csv"),row.names=FALSE,na="")
saveRDS(P,file.path(OUT,"main_analytic_participant_level.rds"))

# ---- 5. models ----
fit<-function(dat,outcome,exposure="V_status",adj=NULL){
  keep<-adj[sapply(adj,function(a) length(unique(dat[[a]][!is.na(dat[[a]])]))>1)]
  dat<-dat[!is.na(dat[[outcome]])&!is.na(dat[[exposure]]),,drop=FALSE]
  if(length(keep)) dat<-dat[complete.cases(dat[,c(outcome,exposure,keep)]),,drop=FALSE]
  mo<-tryCatch(glm(as.formula(paste(outcome,"~",paste(c(exposure,keep),collapse=" + "))),data=dat,family=binomial()),error=function(e)NULL)
  n<-nrow(dat);cs<-sum(dat[[outcome]]==1);ct<-sum(dat[[outcome]]==0)
  if(is.null(mo)) return(list(ok=FALSE,n=n,cases=cs,controls=ct))
  ci<-suppressWarnings(tryCatch(confint(mo),error=function(e)confint.default(mo)))
  co<-coef(mo);en<-grep(paste0("^",exposure),names(co),value=TRUE)[1]
  or<-exp(co[en]);lo<-exp(ci[en,1]);hi<-exp(ci[en,2]);ve<-ve_from_or(or,lo,hi)
  list(ok=TRUE,n=n,cases=cs,controls=ct,adj=keep,OR=or,OR_L=lo,OR_U=hi,VE=ve["VE"],VE_L=ve["VE_L"],VE_U=ve["VE_U"])}
crude<-fit(P,"Y_episode","V_status"); adj<-fit(P,"Y_episode","V_status",c("agegrp","sex_f","caltime"))
outdefs<-c(`Episode (primary)`="Y_episode",`First`="Y_first",`Final`="Y_final",`Any-positive`="Y_any")
sens<-do.call(rbind,lapply(names(outdefs),function(nm){oc<-outdefs[[nm]];mm<-fit(P,oc,"V_status");d<-P[!is.na(P[[oc]])&!is.na(P$V_status),]
  data.frame(`Outcome definition`=nm,N=mm$n,Cases=mm$cases,Controls=mm$controls,`Vaccinated cases`=sum(d[[oc]]==1&d$V_status==1),`Vaccinated controls`=sum(d[[oc]]==0&d$V_status==1),
    OR=sprintf("%.2f",mm$OR),`95% CI`=sprintf("%.2f-%.2f",mm$OR_L,mm$OR_U),`VE %`=sprintf("%.1f",mm$VE),`VE 95% CI`=sprintf("%.1f to %.1f",mm$VE_L,mm$VE_U),check.names=FALSE)}))
model_tbl<-data.frame(Model=c("Crude (vaccinated vs unvaccinated)","Adjusted (age, sex, calendar time)"),
  N=c(crude$n,adj$n),Cases=c(crude$cases,adj$cases),Controls=c(crude$controls,adj$controls),
  OR=sprintf("%.2f",c(crude$OR,adj$OR)),`OR 95% CI`=c(sprintf("%.2f-%.2f",crude$OR_L,crude$OR_U),sprintf("%.2f-%.2f",adj$OR_L,adj$OR_U)),
  `VE %`=sprintf("%.1f",c(crude$VE,adj$VE)),`VE 95% CI`=c(sprintf("%.1f to %.1f",crude$VE_L,crude$VE_U),sprintf("%.1f to %.1f",adj$VE_L,adj$VE_U)),check.names=FALSE)

# ---- 6. ACCOUNTING (reconciles every number) ----
sv$pk<-pkey(sv$name_1,sv$name_4,sv$name_5); mk<-unique(mb$pk)
acc<-data.frame(check.names=FALSE, Quantity=c(
  "linked_dataset_MAIN rows (surv<->lab accepted pairs)",
  "  distinct lab patients (patient_uid)",
  "  distinct surveillance PARTICIPANTS (canonical key) = analysis base",
  "  duplicate MAIN rows collapsed",
  "Surveillance records (raw)",
  "  surveillance records linked to MAIN",
  "  orphan surveillance records (not in MAIN)",
  "  distinct surveillance participants",
  "     of which in MAIN",
  "     MAIN-only (not in surveillance file)",
  "     orphan participants",
  "Lab enrichment of the MAIN participants:",
  "  with a uid-keyed definitive lab sample history",
  "  using single consolidated result (no dated definitive specimen)",
  "  no definitive lab result at all",
  "  multi-sample (>=2 specimens)",
  "Analytic funnel (participants):",
  "  with a definitive RT-PCR (case/control eligible)",
  "  with known vaccination status (vax/unvax)",
  "  PRIMARY analytic N (both) = crude model N",
  "     cases (PCR+)","     controls (PCR-)"),
  N=c(N_MAIN_ROWS, length(unique(mb$lab_uid)), N_PART, N_MAIN_ROWS-N_PART,
      nrow(sv), sum(sv$pk %in% mk), sum(!(sv$pk %in% mk)),
      length(unique(sv$pk)), sum(unique(sv$pk) %in% mk), sum(!(mk %in% unique(sv$pk))), sum(!(unique(sv$pk) %in% mk)),
      NA, sum(P$lab_source=="Lab register (all samples, uid-keyed)"), sum(P$lab_source=="Merged (single result)"), sum(P$lab_source=="No definitive result"), sum(P$n_global_samples>=2),
      NA, sum(!is.na(P$Y_episode)), sum(P$V_status %in% c(0,1)), crude$n, crude$cases, crude$controls))
write.csv(acc,file.path(OUT,"main_ACCOUNTING.csv"),row.names=FALSE,na="")
write_xlsx(list(Accounting=acc, VE_models=model_tbl, Outcome_sensitivity=sens, Analytic=P),
  file.path(OUT,"main_analysis_tables.xlsx"))

# ---- 6b. report bundle (so the Word report reflects THESE verified numbers) ----
P$vacc_type_lbl<-unname(c("1"="rVSV-ZEBOV (Ervebo)","99"="Unknown type","2"="Other")[P$vacc_type]); P$vacc_type_lbl[is.na(P$vacc_type_lbl)]<-""
P$vacc_strat_lbl<-ifelse(nzchar(P$vacc_strategy),paste0("Strategy code ",P$vacc_strategy),"Unknown/not recorded")
P$link_method<-ifelse(grepl("Tier 1",P$linkage_tier),"Automatic / exact (Tier 1)",ifelse(grepl("Tier 2",P$linkage_tier),"Probabilistic (Tier 2)","Manual / adjudicated (Tier 3-4)"))
P$specimen_date<-P$epi_dt; P$onset_to_swab<-as.numeric(P$specimen_date-P$onset)
P$exposure<-dplyr::case_when(P$vacc_status=="Not vaccinated"~"Unvaccinated",
  P$vacc_status=="Vaccinated"&!is.na(P$vax_to_onset)&P$vax_to_onset>10~"Vaccinated >10d (effective)",
  P$vacc_status=="Vaccinated"&!is.na(P$vax_to_onset)&P$vax_to_onset>=0&P$vax_to_onset<=10~"Vaccinated 0-10d (not yet protected)",
  P$vacc_status=="Vaccinated"&!is.na(P$vax_to_onset)&P$vax_to_onset<0~"Vaccinated after onset (ineligible)",
  P$vacc_status=="Vaccinated"&is.na(P$vax_to_onset)~"Vaccinated, timing unknown",TRUE~"Unknown status")
ft<-function(x,lv=NULL){x<-ifelse(is.na(x)|x=="","Missing",as.character(x));if(is.null(lv))lv<-names(sort(table(x),decreasing=TRUE));tb<-as.integer(table(factor(x,levels=lv)));data.frame(Category=lv,n=tb,`%`=round(100*tb/sum(tb),1),check.names=FALSE)}
np<-function(n,d)sprintf("%d (%.1f%%)",n,ifelse(d>0,100*n/d,0))
P$transition<-with(P,ifelse(n_global_samples<2,ifelse(is.na(first),"",paste0("Single (",substr(first,1,1),")")),
  ifelse(first=="Negative"&final=="Positive","N->P",ifelse(first=="Positive"&final=="Negative","P->N",
  ifelse(first=="Negative"&final=="Negative","N->N",ifelse(first=="Positive"&final=="Positive","P->P","other"))))))
rep_tested<-P[P$n_global_samples>=2,]; trans_tab<-ft(rep_tested$transition,c("N->N","N->P","P->P","P->N","other"))
compl<-function(df,label){n<-nrow(df);vd<-df$vacc_status=="Vaccinated"
  t<-data.frame(Variable=c("RT-PCR result (definitive)","Symptom-onset date","Vaccination status","Vaccination date (among vaccinated)","Age","Sex","Health zone","Health area","Village","Vaccination strategy","Specimen/swab date","Linkage method/tier"),Dataset=label,N=n,
    Complete=c(sum(!is.na(df$Y_episode)),sum(!is.na(df$onset)),sum(df$vacc_status%in%c("Vaccinated","Not vaccinated")),sum(vd&!is.na(df$vacc_date)),sum(!is.na(df$age)),sum(df$sex%in%c("Male","Female")),sum(nzchar(df$health_zone)),sum(nzchar(df$health_area)),sum(nzchar(df$village)),sum(df$vacc_strat_lbl!="Unknown/not recorded"),sum(!is.na(df$specimen_date)),n),
    Denominator=c(n,n,n,sum(vd),n,n,n,n,n,n,n,n),check.names=FALSE)
  t$`Complete n (%)`<-sprintf("%d (%.1f%%)",t$Complete,ifelse(t$Denominator>0,100*t$Complete/t$Denominator,0));t$`Missing n (%)`<-sprintf("%d (%.1f%%)",t$Denominator-t$Complete,ifelse(t$Denominator>0,100*(t$Denominator-t$Complete)/t$Denominator,0));t}
cmp_narrow<-compl(P,"MAIN linked (episode result)"); cmp_broad<-compl(P[P$lab_source=="Lab register (all samples, uid-keyed)",],"MAIN linked (all Global samples)")
cmp_cmp<-data.frame(Variable=cmp_narrow$Variable,narrow=cmp_narrow$`Complete n (%)`,broad=cmp_broad$`Complete n (%)`,check.names=FALSE); cmp_all<-rbind(cmp_narrow,cmp_broad)
dist<-list(pcr=ft(P$episode),vacc=ft(P$vacc_status,c("Vaccinated","Not vaccinated","Unknown")),strat=ft(P$vacc_strat_lbl),sex=ft(P$sex),zone=ft(P$health_zone),
  tier=ft(P$link_method,c("Automatic / exact (Tier 1)","Probabilistic (Tier 2)","Manual / adjudicated (Tier 3-4)")),expo=ft(P$exposure),
  age=data.frame(n=sum(!is.na(P$age)),median=median(P$age,na.rm=TRUE),q1=quantile(P$age,.25,na.rm=TRUE),q3=quantile(P$age,.75,na.rm=TRUE),mean=round(mean(P$age,na.rm=TRUE),1),sd=round(sd(P$age,na.rm=TRUE),1),check.names=FALSE))
byS<-P[!is.na(P$Y_episode),];byS$grp<-ifelse(byS$Y_episode==1,"Case (PCR+)","Control (PCR-)")
gg2<-split(byS,byS$grp);cb<-data.frame(Variable=c("N","Age median (IQR)","Female n (%)","Vaccinated n (%)","Health zone Bulape n (%)"),check.names=FALSE)
for(k in names(gg2)){d<-gg2[[k]];cb[[k]]<-c(as.character(nrow(d)),sprintf("%.0f (%.0f-%.0f)",median(d$age,na.rm=TRUE),quantile(d$age,.25,na.rm=TRUE),quantile(d$age,.75,na.rm=TRUE)),np(sum(d$sex=="Female"),nrow(d)),np(sum(d$vacc_status=="Vaccinated"),nrow(d)),np(sum(d$health_zone=="Bulape"),nrow(d)))}
vc<-as.data.frame.matrix(table(byS$grp[byS$vacc_status%in%c("Vaccinated","Not vaccinated")],byS$vacc_status[byS$vacc_status%in%c("Vaccinated","Not vaccinated")]));vc<-cbind(grp=rownames(vc),vc);rownames(vc)<-NULL
dq<-data.frame(Check=c("Vaccination after symptom onset (implausible)","Specimen before symptom onset","Onset-to-swab > 30 days","Vaccinated but vaccination date missing","Vaccinated but vaccine type unknown","Linked but RT-PCR indeterminate/missing"),
  N=c(sum(P$vax_to_onset<0,na.rm=TRUE),sum(P$onset_to_swab<0,na.rm=TRUE),sum(P$onset_to_swab>30,na.rm=TRUE),sum(P$vacc_status=="Vaccinated"&is.na(P$vacc_date)),sum(P$vacc_status=="Vaccinated"&P$vacc_type_lbl=="Unknown type"),sum(is.na(P$Y_episode))),check.names=FALSE)
intv<-data.frame(Interval=c("Onset-to-swab (days)","Vaccination-to-onset (days)"),n=c(sum(!is.na(P$onset_to_swab)),sum(!is.na(P$vax_to_onset))),
  median=c(median(P$onset_to_swab,na.rm=TRUE),median(P$vax_to_onset,na.rm=TRUE)),
  IQR=c(sprintf("%.0f to %.0f",quantile(P$onset_to_swab,.25,na.rm=TRUE),quantile(P$onset_to_swab,.75,na.rm=TRUE)),sprintf("%.0f to %.0f",quantile(P$vax_to_onset,.25,na.rm=TRUE),quantile(P$vax_to_onset,.75,na.rm=TRUE))),check.names=FALSE)
firth<-function(X,y,mx=200,tol=1e-9){b<-rep(0,ncol(X));for(it in 1:mx){p<-as.vector(plogis(X%*%b));W<-p*(1-p);Wr<-sqrt(W);Xi<-solve(crossprod(X*Wr));Q<-(X*Wr)%*%Xi;h<-rowSums(Q*(X*Wr));st<-Xi%*%crossprod(X,(y-p)+h*(0.5-p));b<-b+st;if(max(abs(st))<tol)break};list(coef=as.vector(b),se=sqrt(diag(Xi)))}
df2<-P[!is.na(P$Y_episode)&!is.na(P$V_status),];mm<-model.matrix(~V_status,df2);fr<-firth(mm,df2$Y_episode);j<-which(colnames(mm)=="V_status")
orf<-exp(fr$coef[j]);lof<-exp(fr$coef[j]-1.96*fr$se[j]);hif<-exp(fr$coef[j]+1.96*fr$se[j]);vf<-ve_from_or(orf,lof,hif)
firth_res<-list(OR=orf,OR_L=lof,OR_U=hif,VE=vf["VE"],VE_L=vf["VE_L"],VE_U=vf["VE_U"],n=nrow(df2),cases=sum(df2$Y_episode==1),controls=sum(df2$Y_episode==0))
model_tbl<-rbind(model_tbl,data.frame(Model="Firth crude (sparse-data sensitivity)",N=firth_res$n,Cases=firth_res$cases,Controls=firth_res$controls,OR=sprintf("%.2f",orf),`OR 95% CI`=sprintf("%.2f-%.2f",lof,hif),`VE %`=sprintf("%.1f",vf["VE"]),`VE 95% CI`=sprintf("%.1f to %.1f",vf["VE_L"],vf["VE_U"]),check.names=FALSE))
osv<-tryCatch(rc(file.path(STD,"orphan_surveillance.csv")),error=function(e)NULL); ol<-tryCatch(rc(file.path(STD,"orphan_lab.csv")),error=function(e)NULL)
audit<-data.frame(Step=c("Surveillance case-investigation records","Merged laboratory patient records","Laboratory sample-level observations (Global)","Confirmed (MAIN) linked participants (canonical key)","  Tier 1 automatic/exact","  Tier 2 probabilistic","  Tier 3-4 manual/adjudicated","With uid-keyed lab sample history","  of which multi-sample (>=2)","Using single consolidated result","Primary analytic (definitive result + known vaccination)","Orphan surveillance participants (not in MAIN)"),
  N=c(nrow(sv),nrow(merg),nrow(g),N_PART,sum(mb$tiern==1,na.rm=TRUE),sum(mb$tiern==2,na.rm=TRUE),sum(mb$tiern%in%c(3,4),na.rm=TRUE),sum(P$lab_source=="Lab register (all samples, uid-keyed)"),sum(P$n_global_samples>=2),sum(P$lab_source=="Merged (single result)"),crude$n,ifelse(is.null(osv),NA,length(unique(pkey(osv$case_id,osv$surname,osv$firstname))))),check.names=FALSE)
thm<-theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank(),plot.title=element_text(face="bold"))
ggs<-function(f,p,w=7,h=4){ggsave(file.path(FIG,f),p,width=w,height=h,dpi=150,bg="white")}
try(ggs("F1_vacc_to_onset.png",ggplot(P[!is.na(P$vax_to_onset),],aes(vax_to_onset))+geom_histogram(binwidth=5,fill="#2C7FB8",colour="white")+geom_vline(xintercept=10,linetype="dashed",colour="#C62828")+labs(title="Vaccination-to-onset interval",subtitle="dashed = 10-day rVSV window",x="Days vaccination to onset",y="Participants")+thm),silent=TRUE)
try(ggs("F2_transitions.png",ggplot(trans_tab[trans_tab$n>0,],aes(reorder(Category,n),n))+geom_col(fill="#756BB1")+geom_text(aes(label=n),hjust=-0.2,size=3.5)+coord_flip()+labs(title="RT-PCR transitions (>=2 Global samples)",x=NULL,y="Participants")+thm),silent=TRUE)
fp<-sens;fp$OR<-as.numeric(fp$OR);fp$L<-as.numeric(sub("-.*","",fp$`95% CI`));fp$U<-as.numeric(sub(".*-","",fp$`95% CI`));fp$def<-factor(fp$`Outcome definition`,levels=rev(fp$`Outcome definition`))
try(ggs("F3_forest_OR.png",ggplot(fp,aes(OR,def))+geom_point(size=2.6,colour="#2C3E82")+geom_errorbarh(aes(xmin=L,xmax=U),height=.18,colour="#2C3E82")+geom_vline(xintercept=1,linetype="dashed",colour="grey40")+scale_x_log10()+labs(title="Odds ratio by outcome definition (MAIN linkage)",subtitle="OR<1 favours protection",x="Odds ratio (log scale)",y=NULL)+thm),silent=TRUE)
md<-cmp_narrow;md$Missing_pct<-100-100*md$Complete/md$Denominator
try(ggs("F4_missingness.png",ggplot(md,aes(reorder(Variable,Missing_pct),Missing_pct))+geom_col(fill="#E6550D")+geom_text(aes(label=sprintf("%.0f%%",Missing_pct)),hjust=-0.15,size=3)+coord_flip()+ylim(0,100)+labs(title="Missingness (MAIN linked)",x=NULL,y="% missing")+thm),silent=TRUE)
orph_pos<-ifelse(is.null(ol),NA,sum(grepl("^pos",tolower(blank(ol$pcr_result)))))
results<-list(N=list(sv_raw=nrow(sv),lab_raw=nrow(merg),samp_raw=nrow(g),surv_unique=length(unique(sv$pk)),dup=N_MAIN_ROWS-N_PART,linked=N_PART,analytic=crude$n,cases=crude$cases,controls=crude$controls,rep_tested=nrow(rep_tested),discordant=sum(rep_tested$transition%in%c("N->P","P->N","other")),orph_pos=orph_pos,orph_surv=ifelse(is.null(osv),NA,nrow(osv))),
  audit=audit,cmp_narrow=cmp_narrow,cmp_broad=cmp_broad,cmp_cmp=cmp_cmp,cmp_all=cmp_all,dist=dist,char_by_pcr=cb,vc_tab=vc,trans_tab=trans_tab,model_tbl=model_tbl,sens=sens,dq=dq,interval_summ=intv,
  crude=crude,adj=adj,eff=list(OR=NA,OR_L=NA,OR_U=NA,VE=NA,VE_L=NA,VE_U=NA,n=NA,cases=NA,controls=NA),firth=firth_res)
saveRDS(results,file.path(OUT,"results_bundle_main.rds"))

cat("================= EVE MAIN ANALYSIS — VERIFIED ACCOUNTING =================\n")
print(acc, right=FALSE, row.names=FALSE)
cat(sprintf("\nCrude  OR %.2f (%.2f-%.2f)  VE %.1f%% (%.1f to %.1f)\n",crude$OR,crude$OR_L,crude$OR_U,crude$VE,crude$VE_L,crude$VE_U))
cat(sprintf("Adjust OR %.2f (%.2f-%.2f)  VE %.1f%% (%.1f to %.1f)\n",adj$OR,adj$OR_L,adj$OR_U,adj$VE,adj$VE_L,adj$VE_U))
cat("\nSaved: main_analytic_participant_level.csv/.rds, main_ACCOUNTING.csv, main_analysis_tables.xlsx ->",OUT,"\n")
