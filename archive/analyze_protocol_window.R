# =============================================================================
#  analyze_protocol_window.R
#  Protocol-compliant VE re-analysis (test-negative design) on the coded
#  analytic dataset, plus the variables not analysed in Report 001:
#  vaccination details, hospitalisation, contacts and funeral/burial participation.
#
#  Responds to reviewer comment: VE must follow the protocol exposure
#  definition (Ervebo: symptom onset MORE THAN 10 days after vaccination).
#    - Exposed (V=1)   : vaccinated, onset > 10 days after vaccination
#    - Unexposed (V=0) : not vaccinated
#    - Excluded        : vaccinated 0-10 days before onset (analysed separately),
#                        vaccinated after onset, vaccinated with unknown date,
#                        unknown vaccination status (protocol: not imputed)
#
#  IN :  EVE_VE_Analysis/Distinct_CODED.csv   (built by build_distinct_coded.R)
#  OUT:  EVE_VE_Analysis/main_output_protocol/protocol_analysis_tables.xlsx
#        EVE_VE_Analysis/main_output_protocol/figures/*.png
#  Optional command-line args (for testing on mock data): <IN csv> <OUT dir>
#  All numbers are computed from the coded file; nothing hard-coded.
# =============================================================================
suppressWarnings(suppressMessages({library(dplyr); library(ggplot2); library(writexl)}))
args <- commandArgs(trailingOnly=TRUE)
ROOT <- "C:/Users/hp/Documents/data/EVE_VE_Analysis"
IN   <- if (length(args) >= 1) args[1] else file.path(ROOT,"Distinct_CODED.csv")
OUT  <- if (length(args) >= 2) args[2] else file.path(ROOT,"main_output_protocol")
FIG  <- file.path(OUT,"figures"); dir.create(FIG,showWarnings=FALSE,recursive=TRUE)
stopifnot(file.exists(IN))

D <- read.csv(IN, colClasses="character", check.names=FALSE, na.strings=character(0), encoding="UTF-8"); D[is.na(D)]<-""
numv<-function(x){ suppressWarnings(as.numeric(x)) }
asDate<-function(x){ suppressWarnings(as.Date(ifelse(x=="",NA,x))) }
ve_from_or<-function(or,lo,hi){ or<-unname(or); lo<-unname(lo); hi<-unname(hi); c(VE=(1-or)*100, VE_L=(1-hi)*100, VE_U=(1-lo)*100) }
col_or_blank<-function(v) if (v %in% names(D)) D[[v]] else rep("",nrow(D))
# Yes/No/Unknown recode robust to coded (1/2/99) or text (Yes/Oui/No/Non) source values
yn<-function(x){ x<-toupper(trimws(x))
  ifelse(x %in% c("1","YES","OUI","Y","O","TRUE"),"Yes", ifelse(x %in% c("2","NO","NON","N","FALSE","0"),"No","Unknown")) }

# ---- outcome / exposure / covariates (same definitions as analyze_coded_report.R) ----
D$age      <- numv(D$age)
D$onset    <- asDate(D$onset_date)
D$vacc_dt  <- asDate(D$vacc_date1)
D$Y_episode<- ifelse(D$episode_result_code=="1",1L, ifelse(D$episode_result_code=="0",0L,NA_integer_))
D$V_status <- ifelse(D$vacc_status_code=="1",1L, ifelse(D$vacc_status_code=="2",0L,NA_integer_))
D$agegrp   <- cut(D$age, breaks=c(-Inf,5,17,44,Inf), labels=c("0-5","6-17","18-44","45+"))
D$sex_f    <- factor(ifelse(D$sex %in% c("Male","Female"), D$sex, NA), levels=c("Female","Male"))
D$caltime  <- as.numeric(D$onset - suppressWarnings(min(D$onset,na.rm=TRUE)))/7   # weeks since first onset
D$vax_to_onset <- as.numeric(D$onset - D$vacc_dt)
D$exposure <- dplyr::case_when(
  D$vacc_status=="Not vaccinated" ~ "Unvaccinated",
  D$vacc_status=="Vaccinated" & !is.na(D$vax_to_onset) & D$vax_to_onset>10 ~ "Vaccinated >10d (effective)",
  D$vacc_status=="Vaccinated" & !is.na(D$vax_to_onset) & D$vax_to_onset>=0 & D$vax_to_onset<=10 ~ "Vaccinated 0-10d (not yet protected)",
  D$vacc_status=="Vaccinated" & !is.na(D$vax_to_onset) & D$vax_to_onset<0 ~ "Vaccinated after onset (ineligible)",
  D$vacc_status=="Vaccinated" & is.na(D$vax_to_onset) ~ "Vaccinated, timing unknown",
  TRUE ~ "Unknown status")
EXPO_LV <- c("Unvaccinated","Vaccinated >10d (effective)","Vaccinated 0-10d (not yet protected)",
             "Vaccinated after onset (ineligible)","Vaccinated, timing unknown","Unknown status")

# ---- exposure definitions ----
# PRIMARY (protocol): >10 days vs unvaccinated; every other category excluded
D$V_protocol <- ifelse(D$exposure=="Vaccinated >10d (effective)",1L, ifelse(D$exposure=="Unvaccinated",0L,NA_integer_))
# SEPARATE (protocol): vaccinated 0-10 days before onset vs unvaccinated
D$V_early    <- ifelse(D$exposure=="Vaccinated 0-10d (not yet protected)",1L, ifelse(D$exposure=="Unvaccinated",0L,NA_integer_))
# SENSITIVITY: vaccinated after onset counted as unvaccinated for this episode
D$V_reclass  <- ifelse(D$exposure=="Vaccinated >10d (effective)",1L,
                ifelse(D$exposure %in% c("Unvaccinated","Vaccinated after onset (ineligible)"),0L,NA_integer_))

# ---- variables not analysed in Report 001 ----
D$hosp      <- yn(col_or_blank("ever_hospitalised"))
D$n_hosp    <- numv(col_or_blank("n_hospitalisations"))
D$contact_k <- yn(col_or_blank("contact_known_code"))
D$contact_c <- yn(col_or_blank("contact_with_case_code"))
D$funeral   <- yn(col_or_blank("funeral_code"))
D$travel    <- yn(col_or_blank("travel_code"))
D$vcard     <- yn(col_or_blank("vacc_card_code"))
D$hcw       <- ifelse(col_or_blank("hcw_frontline")=="Yes","Yes","Unknown")
D$prior     <- yn(col_or_blank("prior_ebod_code"))
D$died      <- ifelse(D$vital_status=="Dead","Yes",ifelse(D$vital_status=="Alive","No","Unknown"))
D$vproduct  <- ifelse(nzchar(col_or_blank("vacc_product")), col_or_blank("vacc_product"), "Not recorded")
D$vsource   <- ifelse(nzchar(col_or_blank("vacc_source_code")), paste0("Code ",col_or_blank("vacc_source_code")), "Not recorded")
D$vstrategy <- ifelse(nzchar(col_or_blank("vacc_strategy_code")), paste0("Code ",col_or_blank("vacc_strategy_code")), "Not recorded")
# exposure-risk covariates for adjustment: Unknown kept as its own level so participants are not dropped
D$contact_adj <- factor(D$contact_c, levels=c("No","Yes","Unknown"))
D$funeral_adj <- factor(D$funeral,   levels=c("No","Yes","Unknown"))

D_wp <- D[D$is_cross_record!="Yes", , drop=FALSE]   # within-pair links only (sensitivity)

# =============================================================================
# MODEL HELPERS
# =============================================================================
prep<-function(dat,outcome,exposure,adj){
  dat<-dat[!is.na(dat[[outcome]])&!is.na(dat[[exposure]]),,drop=FALSE]
  keep<-adj[vapply(adj,function(a) length(unique(dat[[a]][!is.na(dat[[a]])]))>1,logical(1))]
  if(length(keep)) dat<-dat[complete.cases(dat[,c(outcome,exposure,keep),drop=FALSE]),,drop=FALSE]
  list(dat=dat,keep=keep)}
cells<-function(d,outcome,exposure) c(vc=sum(d[[outcome]]==1&d[[exposure]]==1), vk=sum(d[[outcome]]==0&d[[exposure]]==1),
                                      uc=sum(d[[outcome]]==1&d[[exposure]]==0), uk=sum(d[[outcome]]==0&d[[exposure]]==0))
# standard logistic regression, profile-likelihood CI
fit_glm<-function(dat,outcome,exposure,adj=character(0)){
  p<-prep(dat,outcome,exposure,adj); d<-p$dat; cc<-cells(d,outcome,exposure)
  res<-list(method="Logistic (profile-likelihood CI)",n=nrow(d),cells=cc,adj=p$keep,OR=NA,OR_L=NA,OR_U=NA)
  if(any(cc==0) || nrow(d)<5) { res$note<-"Zero cell: standard logistic not estimable — see Firth/exact"; return(res) }
  mo<-tryCatch(glm(as.formula(paste(outcome,"~",paste(c(exposure,p$keep),collapse=" + "))),data=d,family=binomial()),error=function(e)NULL)
  if(is.null(mo)) { res$note<-"Model failed"; return(res) }
  ci<-suppressMessages(suppressWarnings(tryCatch(confint(mo),error=function(e)confint.default(mo))))
  res$OR<-unname(exp(coef(mo)[exposure])); res$OR_L<-unname(exp(ci[exposure,1])); res$OR_U<-unname(exp(ci[exposure,2]))
  res$note<-if(abs(coef(mo)[exposure])>10) "Possible separation" else ""
  res}
# Firth penalised logistic regression (Jeffreys prior), Wald CI on penalised scale
firth_core<-function(X,y,mx=200,tol=1e-9){b<-rep(0,ncol(X));for(it in 1:mx){p<-as.vector(plogis(X%*%b));W<-p*(1-p);Wr<-sqrt(W)
  Xi<-solve(crossprod(X*Wr));Q<-(X*Wr)%*%Xi;h<-rowSums(Q*(X*Wr));st<-Xi%*%crossprod(X,(y-p)+h*(0.5-p));b<-b+st;if(max(abs(st))<tol)break}
  list(coef=as.vector(b),se=sqrt(diag(Xi)))}
fit_firth<-function(dat,outcome,exposure,adj=character(0)){
  p<-prep(dat,outcome,exposure,adj); d<-p$dat; cc<-cells(d,outcome,exposure)
  res<-list(method="Firth penalised",n=nrow(d),cells=cc,adj=p$keep,OR=NA,OR_L=NA,OR_U=NA,note="")
  if(nrow(d)<5 || sum(d[[exposure]])==0 || sum(d[[exposure]]==0)==0) { res$note<-"Not estimable"; return(res) }
  X<-model.matrix(as.formula(paste("~",paste(c(exposure,p$keep),collapse="+"))),d)
  fr<-tryCatch(firth_core(X,d[[outcome]]),error=function(e)NULL); if(is.null(fr)) { res$note<-"Firth failed"; return(res) }
  j<-which(colnames(X)==exposure); b<-unname(fr$coef[j]); se<-unname(fr$se[j])
  res$OR<-exp(b); res$OR_L<-exp(b-1.96*se); res$OR_U<-exp(b+1.96*se)
  if(any(cc==0)) res$note<-"Zero cell: Firth estimate shown"
  res}
# crude exact (Fisher conditional MLE): valid even with a zero cell
fit_exact<-function(dat,outcome,exposure){
  d<-dat[!is.na(dat[[outcome]])&!is.na(dat[[exposure]]),,drop=FALSE]; cc<-cells(d,outcome,exposure)
  res<-list(method="Exact (Fisher conditional MLE)",n=nrow(d),cells=cc,adj=character(0),OR=NA,OR_L=NA,OR_U=NA,note="")
  if(nrow(d)<2) return(res)
  ft<-tryCatch(fisher.test(matrix(c(cc["vc"],cc["uc"],cc["vk"],cc["uk"]),2)),error=function(e)NULL)
  if(!is.null(ft)){res$OR<-unname(ft$estimate);res$OR_L<-ft$conf.int[1];res$OR_U<-ft$conf.int[2];res$p<-ft$p.value}
  res}
row_of<-function(label,group,r){
  ve<-ve_from_or(r$OR,r$OR_L,r$OR_U); f2<-function(x) ifelse(is.na(x)|!is.finite(x),"NA",sprintf("%.2f",x)); f1<-function(x) ifelse(is.na(x)|!is.finite(x),"NA",sprintf("%.1f",x))
  data.frame(Group=group, Model=label, Method=r$method,
    `Adjusted for`=if(length(r$adj)) paste(r$adj,collapse=", ") else "—",
    N=r$n, Cases=unname(r$cells["vc"]+r$cells["uc"]), Controls=unname(r$cells["vk"]+r$cells["uk"]),
    `Vaccinated cases`=unname(r$cells["vc"]), `Vaccinated controls`=unname(r$cells["vk"]),
    `Unvaccinated cases`=unname(r$cells["uc"]), `Unvaccinated controls`=unname(r$cells["uk"]),
    OR=f2(r$OR), `OR 95% CI`=paste0(f2(r$OR_L)," to ",f2(r$OR_U)),
    `VE %`=f1(ve["VE"]), `VE 95% CI`=paste0(f1(ve["VE_L"])," to ",f1(ve["VE_U"])),
    Significant=ifelse(is.na(r$OR_U),"",ifelse(r$OR_U<1,"Yes","No")),
    Note=if(is.null(r$note)) "" else r$note, check.names=FALSE)}

ADJ    <- c("agegrp","sex_f","caltime")
ADJ_EX <- c(ADJ,"contact_adj","funeral_adj")

# =============================================================================
# 1. VE MODELS
# =============================================================================
P <- "Protocol (>10 days vs unvaccinated) — PRIMARY"
models <- rbind(
  row_of("Crude",                                         P, fit_glm  (D,"Y_episode","V_protocol")),
  row_of("Crude, exact",                                  P, fit_exact(D,"Y_episode","V_protocol")),
  row_of("Firth crude",                                   P, fit_firth(D,"Y_episode","V_protocol")),
  row_of("Adjusted (age, sex, calendar time)",            P, fit_glm  (D,"Y_episode","V_protocol",ADJ)),
  row_of("Firth adjusted (age, sex, calendar time)",      P, fit_firth(D,"Y_episode","V_protocol",ADJ)),
  row_of("Firth adjusted + exposure risk (contact with case, funeral participation)", P, fit_firth(D,"Y_episode","V_protocol",ADJ_EX)),
  row_of("Crude, excluding cross-record links",           P, fit_firth(D_wp,"Y_episode","V_protocol")),
  row_of("Vaccinated 0-10 days vs unvaccinated (crude)",  "Separate analysis (protocol)", fit_firth(D,"Y_episode","V_early")),
  row_of("Vaccinated after onset counted as unvaccinated (crude)", "Sensitivity", fit_firth(D,"Y_episode","V_reclass")),
  row_of("Status-based, all vaccinated (Report 001 definition, crude)", "Comparison with Report 001", fit_glm(D,"Y_episode","V_status")),
  row_of("Status-based + exposure risk (Firth adjusted)", "Comparison with Report 001", fit_firth(D,"Y_episode","V_status",ADJ_EX)))

# =============================================================================
# 2. EXPOSURE CLASSIFICATION BY CASE / CONTROL (who is kept / excluded)
# =============================================================================
byS <- D[!is.na(D$Y_episode),]; byS$grp <- ifelse(byS$Y_episode==1,"Cases (PCR+)","Controls (PCR-)")
expo_tab <- as.data.frame.matrix(table(factor(byS$exposure,levels=EXPO_LV), byS$grp))
expo_tab <- data.frame(`Exposure category`=rownames(expo_tab), expo_tab,
  `In protocol analysis`=ifelse(rownames(expo_tab) %in% c("Unvaccinated","Vaccinated >10d (effective)"),"Yes",
                         ifelse(rownames(expo_tab)=="Vaccinated 0-10d (not yet protected)","Separate analysis","Excluded")), check.names=FALSE)
rownames(expo_tab) <- NULL

# =============================================================================
# 3. CHARACTERISTICS: vaccination, hospitalisation, contacts, funeral participation
# =============================================================================
pfmt<-function(p) ifelse(is.na(p),"",ifelse(p<0.001,"<0.001",sprintf("%.3f",p)))
cat_block<-function(dat,grp,var,label,levels=NULL,test_exclude=c("Unknown","Not recorded")){
  x<-dat[[var]]; g<-dat[[grp]]; G<-sort(unique(g))
  if(is.null(levels)) levels<-names(sort(table(x),decreasing=TRUE))
  # Fisher exact on known categories (protocol: Fisher for nominal variables); simulated p for large tables
  k<-!(x %in% test_exclude); tb<-table(factor(x[k],levels=setdiff(levels,test_exclude)), g[k])
  tb<-tb[rowSums(tb)>0,,drop=FALSE]
  p<-if(nrow(tb)>=2 && ncol(tb)>=2) tryCatch(fisher.test(tb)$p.value,error=function(e)
        tryCatch(fisher.test(tb,simulate.p.value=TRUE,B=1e4)$p.value,error=function(e)NA)) else NA
  out<-data.frame(Variable=c(label,paste0("    ",levels)),check.names=FALSE)
  for(gg in G){ d<-x[g==gg]; out[[gg]]<-c("",sprintf("%d (%.1f)",as.integer(table(factor(d,levels=levels))),100*as.integer(table(factor(d,levels=levels)))/max(1,length(d))))}
  out$`p-value`<-c(pfmt(p),rep("",length(levels))); out}
num_block<-function(dat,grp,var,label){
  x<-dat[[var]]; g<-dat[[grp]]; G<-sort(unique(g))
  p<-tryCatch(wilcox.test(x~g,exact=FALSE)$p.value,error=function(e)NA)   # protocol: Mann-Whitney
  out<-data.frame(Variable=paste0(label,", median (IQR)"),check.names=FALSE)
  for(gg in G){ d<-x[g==gg & !is.na(x)]; out[[gg]]<-if(length(d)) sprintf("%.0f (%.0f–%.0f) [n=%d]",median(d),quantile(d,.25),quantile(d,.75),length(d)) else "—"}
  out$`p-value`<-pfmt(p); out}
YNU<-c("Yes","No","Unknown")
char_table<-function(dat,grp){
  hdr<-data.frame(Variable="N",check.names=FALSE); for(gg in sort(unique(dat[[grp]]))) hdr[[gg]]<-as.character(sum(dat[[grp]]==gg)); hdr$`p-value`<-""
  section<-function(t){ s<-hdr[0,,drop=FALSE]; s[1,]<-c(t,rep("",ncol(hdr)-1)); s }
  rbind(hdr,
    section("DEMOGRAPHICS"),
    num_block(dat,grp,"age","Age (years)"),
    cat_block(dat,grp,"sex","Sex",c("Female","Male","Unknown")),
    section("VACCINATION"),
    cat_block(dat,grp,"vacc_status","Vaccination status",c("Vaccinated","Not vaccinated","Unknown")),
    cat_block(dat,grp,"exposure","Protocol exposure window",EXPO_LV,test_exclude=c("Unknown status")),
    cat_block(dat,grp,"vproduct","Vaccine product"),
    cat_block(dat,grp,"vcard","Vaccination card seen",YNU),
    cat_block(dat,grp,"vsource","Source of vaccination information (coded)"),
    cat_block(dat,grp,"vstrategy","Vaccination strategy (coded; no decode key)"),
    section("CLINICAL"),
    cat_block(dat,grp,"hosp","Ever hospitalised",YNU),
    num_block(dat,grp,"n_hosp","Number of hospitalisations"),
    cat_block(dat,grp,"died","Died",YNU),
    section("EXPOSURE / CONTACT"),
    cat_block(dat,grp,"contact_k","Known contact of a case",YNU),
    cat_block(dat,grp,"contact_c","Contact with a confirmed case",YNU),
    cat_block(dat,grp,"funeral","Funeral / burial participation",YNU),
    cat_block(dat,grp,"travel","Travel history",YNU),
    cat_block(dat,grp,"hcw","Health / frontline worker",c("Yes","Unknown")),
    cat_block(dat,grp,"prior","Prior documented Ebola disease",YNU))}

# 3a. by case / control (all with an episode-defining result)
char_case <- char_table(byS,"grp")
# 3b. by protocol exposure group (primary analytic sample) — shows confounding by exposure risk
PA <- D[!is.na(D$Y_episode)&!is.na(D$V_protocol),]; PA$vgrp<-ifelse(PA$V_protocol==1,"Vaccinated >10d","Unvaccinated")
char_vacc <- char_table(PA,"vgrp")

# =============================================================================
# 4. SEVERE OUTCOMES AMONG CONFIRMED CASES BY VACCINATION (descriptive; secondary objective)
#    Not a VE-against-death estimate: the cohort analysis (HR) needs admission and
#    outcome dates and is planned separately.
# =============================================================================
CS <- D[D$Y_episode %in% 1,]
sev_block<-function(dat,expvar,label){
  dd<-dat[!is.na(dat[[expvar]]),]; if(!nrow(dd)) return(NULL)
  dd$g<-ifelse(dd[[expvar]]==1,"Vaccinated","Unvaccinated")
  do.call(rbind,lapply(c("died","hosp"),function(v){
    k<-dd[[v]]!="Unknown"; tb<-table(factor(dd$g[k],levels=c("Vaccinated","Unvaccinated")),factor(dd[[v]][k],levels=c("Yes","No")))
    p<-tryCatch(fisher.test(tb)$p.value,error=function(e)NA)
    data.frame(Exposure=label,Outcome=ifelse(v=="died","Died","Ever hospitalised"),
      `Vaccinated: n/N (%)`=sprintf("%d/%d (%.1f)",tb["Vaccinated","Yes"],sum(tb["Vaccinated",]),100*tb["Vaccinated","Yes"]/max(1,sum(tb["Vaccinated",]))),
      `Unvaccinated: n/N (%)`=sprintf("%d/%d (%.1f)",tb["Unvaccinated","Yes"],sum(tb["Unvaccinated",]),100*tb["Unvaccinated","Yes"]/max(1,sum(tb["Unvaccinated",]))),
      `Fisher p`=pfmt(p),`Outcome unknown`=sum(!k),check.names=FALSE)}))}
severity <- rbind(sev_block(CS,"V_protocol","Protocol (>10 days vs unvaccinated)"),
                  sev_block(CS,"V_status","Status-based (any recorded vaccination)"))

# =============================================================================
# 5. COMPLETENESS OF THE NEWLY ANALYSED VARIABLES
# =============================================================================
compl_new <- do.call(rbind,lapply(list(c("hosp","Ever hospitalised"),c("contact_k","Known contact of a case"),
  c("contact_c","Contact with a confirmed case"),c("funeral","Funeral / burial participation"),c("vcard","Vaccination card seen"),
  c("travel","Travel history")),function(v){k<-sum(D[[v[1]]]!="Unknown");
  data.frame(Variable=v[2],`Known n (%)`=sprintf("%d (%.1f)",k,100*k/nrow(D)),`Unknown n (%)`=sprintf("%d (%.1f)",nrow(D)-k,100*(nrow(D)-k)/nrow(D)),check.names=FALSE)}))

# =============================================================================
# 6. FIGURES
# =============================================================================
thm<-theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank(),plot.title=element_text(face="bold"))
ggs<-function(f,p,w=8,h=4.5) suppressWarnings(ggsave(file.path(FIG,f),p,width=w,height=h,dpi=150,bg="white"))
fp<-models; fp$VE<-numv(fp$`VE %`); fp$L<-numv(sub(" to .*","",fp$`VE 95% CI`)); fp$U<-numv(sub(".* to ","",fp$`VE 95% CI`))
fp<-fp[!is.na(fp$VE),]; fp$lab<-factor(paste0(fp$Model," (",fp$`Vaccinated cases`,"/",fp$Cases," vacc. cases)"),levels=rev(unique(paste0(fp$Model," (",fp$`Vaccinated cases`,"/",fp$Cases," vacc. cases)"))))
fp$Lp<-pmax(fp$L,-100)
if(nrow(fp)) try(ggs("P1_forest_protocol_VE.png",ggplot(fp,aes(VE,lab,colour=Group))+geom_vline(xintercept=0,linetype="dashed",colour="grey40")+
  geom_errorbar(aes(xmin=Lp,xmax=U),width=.2,orientation="y")+geom_point(size=2.6)+coord_cartesian(xlim=c(-100,100))+
  labs(title="Vaccine effectiveness: protocol exposure definition",subtitle="Lower limits truncated at -100%",x="VE % (95% CI)",y=NULL,colour=NULL)+
  scale_colour_manual(values=c("Protocol (>10 days vs unvaccinated) — PRIMARY"="#1F4E79","Separate analysis (protocol)"="#5B9BD5",
    "Sensitivity"="#E38728","Comparison with Report 001"="#A6A6A6"))+guides(colour=guide_legend(nrow=2))+
  thm+theme(legend.position="bottom"),w=10,h=5.5),silent=TRUE)
ex<-byS%>%select(grp,`Contact with a confirmed case`=contact_c,`Funeral / burial participation`=funeral,`Ever hospitalised`=hosp)%>%
  tidyr::pivot_longer(-grp)%>%count(grp,name,value)%>%group_by(grp,name)%>%mutate(pct=100*n/sum(n))%>%ungroup()
try(ggs("P2_exposure_risk_by_status.png",ggplot(ex,aes(grp,pct,fill=factor(value,levels=YNU)))+geom_col(width=.7)+facet_wrap(~name)+
  scale_fill_manual(values=c(Yes="#1F4E79",No="#9DC3E6",Unknown="#D9D9D9"))+labs(title="Exposure risk and hospitalisation by RT-PCR status",x=NULL,y="% of participants",fill=NULL)+thm),silent=TRUE)

# =============================================================================
# 7. EXPORT + CONSOLE SUMMARY
# =============================================================================
write_xlsx(list(VE_models=models, Exposure_by_status=expo_tab, Char_by_case_control=char_case,
  Char_by_protocol_vaccination=char_vacc, Severity_among_cases=if(is.null(severity)) data.frame() else severity,
  Completeness_new_vars=compl_new), file.path(OUT,"protocol_analysis_tables.xlsx"))

cat("\n========== PROTOCOL-WINDOW VE RE-ANALYSIS ==========\n")
print(models[,c("Group","Model","N","Vaccinated cases","Vaccinated controls","VE %","VE 95% CI","Note")],row.names=FALSE)
cat("\n-- Exposure category by case/control --\n"); print(expo_tab,row.names=FALSE)
vcp<-models$`Vaccinated cases`[1]
if(!is.na(vcp) && vcp<5) cat(sprintf("\nCAUTION: only %d vaccinated case(s) under the protocol definition — report Firth/exact estimates and do not over-interpret.\n",vcp))
cat("\nTables:  ",file.path(OUT,"protocol_analysis_tables.xlsx"),"\nFigures: ",FIG,"\n",sep="")

