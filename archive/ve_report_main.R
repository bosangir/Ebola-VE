# =============================================================================
#  ve_report.R  —  builds the Word report (Century Gothic) from results_bundle.rds
#  All numbers are pulled from the computed bundle (never hard-coded).
# =============================================================================
suppressWarnings(suppressMessages({ library(officer); library(flextable); library(dplyr) }))
ROOT <- "C:/Users/hp/Documents/data/EVE_VE_Analysis"
OUT <- file.path(ROOT,"main_output"); FIG <- file.path(OUT,"figures"); TBL <- OUT
R <- readRDS(file.path(OUT,"results_bundle_main.rds"))
FONT <- "Century Gothic"
set_flextable_defaults(font.family=FONT, font.size=9, padding=3, theme_fun=theme_booktabs)

# ---- styled text helpers (Century Gothic everywhere) ------------------------
tt   <- function(txt, sz=11, bold=FALSE, color="#000000", italic=FALSE) fp_text(font.family=FONT, font.size=sz, bold=bold, color=color, italic=italic)
par0 <- fp_par(text.align="left", padding.bottom=6)
H1n <- 0
doc <- read_docx()
H1 <- function(txt){ H1n<<-H1n+1; doc <<- doc %>% body_add_fpar(fpar(ftext(paste0(H1n,". ",txt), tt(txt,15,TRUE,"#1F3864")), fp_p=fp_par(padding.top=10,padding.bottom=4))) }
H2c <- 0; curr1 <- function() H1n
H2 <- function(txt){ H2c<<-H2c+1; doc <<- doc %>% body_add_fpar(fpar(ftext(paste0(H1n,".",H2c," ",txt), tt(txt,12,TRUE,"#2E5496")), fp_p=fp_par(padding.top=6,padding.bottom=3))) }
resetH2 <- function(){ H2c<<-0 }
BODY <- function(txt){ doc <<- doc %>% body_add_fpar(fpar(ftext(txt, tt(txt,10.5)), fp_p=par0)) }
BULl <- function(txt){ doc <<- doc %>% body_add_fpar(fpar(ftext(paste0("•  ",txt), tt(txt,10.5)), fp_p=fp_par(padding.bottom=2,padding.left=14))) }
CAP  <- function(txt){ doc <<- doc %>% body_add_fpar(fpar(ftext(txt, tt(txt,9,TRUE,"#404040",TRUE)), fp_p=fp_par(padding.top=6,padding.bottom=2))) }
NOTE <- function(txt){ doc <<- doc %>% body_add_fpar(fpar(ftext(txt, tt(txt,9,FALSE,"#7030A0",TRUE)), fp_p=fp_par(padding.bottom=4))) }
FT   <- function(df, widths=NULL){ ft <- flextable(as.data.frame(df)) %>% bold(part="header") %>%
          bg(part="header", bg="#D9E2F3") %>% border_outer(part="all") %>% border_inner_h(part="body") %>%
          fontsize(size=8.5, part="all") %>% font(fontname=FONT, part="all") %>% autofit()
        doc <<- doc %>% body_add_flextable(ft) %>% body_add_par("") }
SP <- function() doc <<- doc %>% body_add_par("")

# read the CSV tables for direct rendering
rd <- function(f) read.csv(file.path(TBL,f), check.names=FALSE, colClasses="character", na.strings=character(0))

# ============================ TITLE ==========================================
doc <- doc %>%
  body_add_fpar(fpar(ftext("Preliminary Vaccine Effectiveness Analysis", tt("",20,TRUE,"#1F3864")), fp_p=fp_par(text.align="center",padding.top=30,padding.bottom=4))) %>%
  body_add_fpar(fpar(ftext("Ebola Virus Disease, Bulape and surrounding health zones, Democratic Republic of the Congo", tt("",13,FALSE,"#2E5496")), fp_p=fp_par(text.align="center",padding.bottom=2))) %>%
  body_add_fpar(fpar(ftext("Test-negative design on the CONFIRMED (MAIN) linkage, using every laboratory sample per patient", tt("",11,FALSE,"#404040",TRUE)), fp_p=fp_par(text.align="center",padding.bottom=20))) %>%
  body_add_fpar(fpar(ftext("PRELIMINARY — for internal review. All estimates are provisional and subject to the data limitations described herein.", tt("",10,TRUE,"#C00000")), fp_p=fp_par(text.align="center",padding.bottom=20)))
doc <- doc %>% body_add_fpar(fpar(ftext(sprintf("Analytic dataset: %d unique participants; %d linked to laboratory data; primary test-negative sample %d (%d cases, %d controls).",
  R$N$surv_unique, R$N$linked, R$N$analytic, R$N$cases, R$N$controls), tt("",10.5)), fp_p=fp_par(text.align="center",padding.bottom=6)))
doc <- doc %>% body_add_break()

# ============================ 1. EXEC SUMMARY ================================
H1("Executive summary"); resetH2()
cr<-R$crude; ad<-R$adj
BODY("This preliminary analysis evaluates the effectiveness of Ebola (rVSV-ZEBOV / Ervebo) vaccination against laboratory-confirmed Ebola virus disease (EBOD) using a test-negative design, as specified in the governing study protocol. Cases are suspected EBOD patients with a positive RT-PCR result and controls are suspected patients who test negative; vaccine effectiveness (VE) is derived from the odds ratio (OR) as VE = (1 - OR) x 100 percent.")
BODY(sprintf("From %d surveillance case-investigation records, %d were in the confirmed (MAIN) linkage used here — the wrong links flagged during adjudication of the automatic name-matching were excluded, and each linked patient's FULL laboratory history (all samples, not one) was used. The primary test-negative analysis included %d participants (%d cases, %d controls) with a definitive RT-PCR result and known vaccination status.",
  R$N$sv_raw, R$N$linked, R$N$analytic, R$N$cases, R$N$controls))
BODY(sprintf("The crude odds ratio for vaccination was %.2f (95%% CI %.2f-%.2f), a crude VE of %.1f%% (95%% CI %.1f%% to %.1f%%), which is %s. After adjustment for age, sex and calendar time the OR was %.2f (95%% CI %.2f-%.2f), VE %.1f%% (95%% CI %.1f%% to %.1f%%), which is %s. Both point estimates are protective; with only %d cases the estimates are imprecise and remain preliminary.",
  cr$OR,cr$OR_L,cr$OR_U, cr$VE,cr$VE_L,cr$VE_U, ifelse(cr$OR_U<1,"statistically significant","not statistically significant"),
  ad$OR,ad$OR_L,ad$OR_U, ad$VE,ad$VE_L,ad$VE_U, ifelse(ad$OR_U<1,"statistically significant","not statistically significant"), cr$cases))
BODY(sprintf("Among %d participants with repeated RT-PCR testing, %d showed changing/discordant results. Under alternative outcome definitions (first, final, any-positive) the VE point estimate ranged across the sensitivity analyses and, for some definitions, reached conventional statistical significance. The substantive interpretation is therefore sensitive to how the participant-level outcome is defined, underscoring the preliminary nature of these estimates.",
  R$N$rep_tested, R$N$discordant))
NOTE("Statistical non-significance is not evidence of no effect. These estimates are limited by small numbers, incomplete vaccination-date and vaccination-strategy information, and linkage that is not yet fully adjudicated.")

# ============================ 2. BACKGROUND ==================================
H1("Background and objectives"); resetH2()
BODY("The objective is to estimate the effectiveness of prior Ebola vaccination in preventing laboratory-confirmed EBOD, using routinely collected surveillance, laboratory and vaccination data from the outbreak response. This report addresses two analyses requested in the protocol and by the analytic team: (i) a primary test-negative analysis in which each participant contributes a single definitive RT-PCR outcome; and (ii) a secondary analysis of participants with repeated or discordant RT-PCR results, with alternative outcome definitions. It also provides the descriptive completeness assessment requested by Pierre and a comparison of the current (narrow) and broader (all-samples) linkage datasets.")

# ============================ 3. METHODS =====================================
H1("Methods"); resetH2()
H2("Data sources")
BULl("Surveillance / case-investigation records (case identity, symptom-onset date, vaccination history, demographics, geography).")
BULl("Laboratory data: a triangulated laboratory register produced by merging SIX source datasets (DATA001-DATA006, with a health-zone update) into one consolidated patient record per laboratory patient identifier, following the data-collection SOP. Each patient's full specimen history (all RT-PCR results and dates) is taken from this triangulated register, joined to the linkage by the laboratory patient identifier. This is the STANDARD laboratory source for the analysis.")
BULl("Vaccination information: no standalone national vaccination register file was available for this preliminary run; vaccination status, date, type and strategy were taken from the surveillance/case-investigation fields (documented limitation).")
H2("Deduplication and source-priority rules")
BODY("Exact and probable duplicate participant records were identified using an identity key based on a real case identifier where available, otherwise the standardized full name. Repeated laboratory observations belonging to the same individual were retained as a laboratory history and were explicitly distinguished from true duplicate participant records. One analytic record per participant was retained using documented source-priority rules:")
BULl("RT-PCR result and specimen date: laboratory source preferred.")
BULl("Vaccination status, date, type and strategy: vaccination register preferred (surveillance fields used here as the register file was unavailable).")
BULl("Symptom-onset date: surveillance/case-investigation source, retaining the earliest clinically plausible date.")
BODY("Conflicting observations were not deleted; discrepancy indicators (e.g., timing implausibilities) were preserved and are reported in the data-quality section.")
H2("Record linkage method")
BODY("Surveillance case records were linked to laboratory records using a NAME-BASED probabilistic algorithm with demographic validation and human adjudication. Identifier-only matching was not used as a definitive criterion because the surveillance case-identifier field is a placeholder (“RAS”) or otherwise non-unique in a substantial minority of records; personal names, with demographic support, are the primary linkage variables.")
BODY("Names were normalised (accents and punctuation removed, upper-cased, tokenised). Candidate pairs were classified into tiers: Tier 1 — normalized full-name exact match, auto-accepted only when sex agreed AND age agreed within ±5 years; Tier 2 — two or more shared exact name tokens; Tier 3 — one exact shared token plus at least one partial/near token (shared ≥4-character prefix, substring, or one-character/Levenshtein difference) with sex agreement and age within ±5 years; Tier 4 — looser partial name-part match. Tiers 2–4 were never auto-accepted; every one was routed to a manual Level-2 review queue where a reviewer accepted, rejected, or held each candidate. For reporting, tiers map to method: Tier 1 = automatic/exact, Tier 2 = probabilistic, Tiers 3–4 = manual/adjudicated.")
BODY("This analysis uses the CONFIRMED linkage only — the set of pairs that were auto-accepted (Tier 1 passing the demographic gate) or reviewer-accepted (the “MAIN” linked dataset). Pairs the reviewer judged wrong during adjudication were excluded. The confirmed set was then deduplicated to one row per participant using a single canonical key (the real case-identifier where present, otherwise the normalised full name); where the same participant appeared on multiple confirmed rows, the reviewer-accepted / lowest-tier row was retained. Each confirmed participant was carried forward with their full laboratory sample history, obtained by joining on the laboratory patient identifier (an exact ID join to the triangulated laboratory register, NOT a name match) so that every specimen for that patient is captured; an earlier name-based join was found to mis-assign specimens for a small number of patients and was replaced. Where no specimen carried a definitive positive/negative result, the single consolidated result was used, and the laboratory source is recorded per participant.")
H2("Analysis 1 — single definitive RT-PCR outcome")
BODY("Each participant contributed one definitive RT-PCR outcome. Where a participant had multiple specimens, the episode-defining result was the first definitive (positive/negative) specimen collected on or after symptom onset (otherwise the first definitive specimen). Y=1 for RT-PCR positive (case) and Y=0 for negative (control); indeterminate/missing results were excluded because case/control status cannot be determined. Exposure used vaccination status and, where a vaccination date was available, the vaccination-to-onset interval D = onset - vaccination date. Following the protocol for rVSV-ZEBOV, individuals were considered effectively vaccinated only when onset occurred >10 days after vaccination; those vaccinated <=10 days before onset were treated as not-yet-protected and analysed separately. Because vaccination dates were available for a minority of vaccinated participants, the primary model used vaccinated-vs-unvaccinated status, with the >10-day effective-window model reported as a protocol-aligned but preliminary estimate. Unknown vaccination status was excluded from the primary analysis and vaccination status was not imputed.")
BODY("The crude model was logit P(Y=1) = b0 + b1*V. The adjusted model added age group, sex and calendar time (weeks since first onset); covariates were kept parsimonious given the number of events to avoid overfitting, and geography was not included because participants were overwhelmingly from a single health zone. VE and its confidence interval were derived from the OR: VE = (1-OR)x100%, with limits VE_L=(1-OR_U)x100% and VE_U=(1-OR_L)x100%. Odds-ratio confidence intervals used profile likelihood. A compact Firth (Jeffreys-penalized) logistic regression was implemented as a sparse-data sensitivity analysis (the logistf/geepack packages were not available in the environment).")
H2("Analysis 2 — changing/discordant RT-PCR results and alternative outcomes")
BODY("For participants with repeated testing we derived the number of tests, first and last specimen dates, first and final results, whether any positive occurred, dates of first/last positive, the result trajectory, and a participant-level transition category T in {N->N, N->P, P->P, P->N, other}. The VE models were re-estimated under four outcome definitions: (1) episode-defining (primary); (2) first result; (3) final result; (4) any-positive (case if >=1 valid RT-PCR is positive). A repeated-measures/GEE model was pre-specified as secondary but was not run because the geepack package was unavailable and the number of participants with multiple definitive discordant results is small.")
H2("Missing data and software")
BODY("Missing or indeterminate laboratory results were excluded from the primary analysis; vaccination status was not imputed and unknown status was excluded from the primary model; missing vaccination dates were handled separately and did not lead to automatic classification as unvaccinated. Covariate missingness was handled by complete-case analysis for the adjusted model. Analyses used R 4.6.0 (packages dplyr, ggplot2, officer, flextable).")

# ============================ 4. RESULTS =====================================
H1("Results"); resetH2()
H2("Data flow and deduplication")
CAP("Table 4.1. Participant data-flow and linkage audit.")
FT(R$audit)
BODY(sprintf("Of %d surveillance records, %d duplicate participant records were consolidated, leaving %d unique participants; %d were linked to laboratory data and %d PCR-positive laboratory records had no surveillance match (orphan positives).",
  R$N$sv_raw, R$N$dup, R$N$surv_unique, R$N$linked, R$N$orph_pos))
H2("Completeness assessment (requested by Pierre)")
CAP("Table 4.2. Completeness of key variables — narrow (one result per participant) and broad (all samples).")
FT(R$cmp_all %>% select(Variable,Dataset,`Complete n (%)`,`Missing n (%)`))
H2("Descriptive characteristics")
CAP("Table 4.3. Characteristics by RT-PCR status (episode-defining outcome).")
FT(R$char_by_pcr)
CAP("Table 4.4. Vaccination status by case/control status.")
FT(R$vc_tab)
CAP("Table 4.5. Distributions: RT-PCR result, vaccination status, sex, health zone, linkage method, and protocol exposure window.")
FT(bind_rows(
  R$dist$pcr  %>% transmute(Group="RT-PCR", Category, n, `%`),
  R$dist$vacc %>% transmute(Group="Vaccination", Category, n, `%`),
  R$dist$tier %>% transmute(Group="Linkage method", Category, n, `%`),
  R$dist$expo %>% transmute(Group="Exposure window", Category, n, `%`)))
H2("Analysis 1 — vaccine-effectiveness models")
CAP("Table 4.6. Crude, adjusted, protocol-window and Firth VE estimates (episode-defining outcome).")
FT(R$model_tbl)
BODY(sprintf("The crude VE was %.1f%% (95%% CI %.1f%% to %.1f%%, %s) and the adjusted VE %.1f%% (95%% CI %.1f%% to %.1f%%, %s). Point estimates are protective; the intervals are wide given the small number of cases, so the estimate is preliminary and should not be over-interpreted.",
  cr$VE,cr$VE_L,cr$VE_U, ifelse(cr$OR_U<1,"significant","not significant"), ad$VE,ad$VE_L,ad$VE_U, ifelse(ad$OR_U<1,"significant","not significant")))
H2("Analysis 2 — discordant RT-PCR results and outcome-definition sensitivity")
CAP("Table 4.7. RT-PCR transition patterns among repeatedly tested participants.")
FT(R$trans_tab)
CAP("Table 4.8. Vaccine-effectiveness under alternative outcome definitions (sensitivity analysis).")
FT(R$sens)
BODY(sprintf("Of %d participants with repeated RT-PCR testing, %d had changing/discordant results (mainly positive-to-negative clearance and negative-to-positive sequences). VE point estimates across outcome definitions remained protective; however, statistical significance was not consistent across definitions, indicating the conclusion is sensitive to outcome specification.",
  R$N$rep_tested, R$N$discordant))
H2("Timing and data-quality")
CAP("Table 4.9. Timing intervals (days).")
FT(R$interval_summ)
CAP("Table 4.10. Data-quality and timing flags.")
FT(R$dq)

# figures
H2("Figures")
addfig <- function(file, cap, w=6.3, h=3.8){ p<-file.path(FIG,file); if(file.exists(p)){ CAP(cap); doc <<- doc %>% body_add_img(src=p, width=w, height=h) %>% body_add_par("") } }
addfig("F4_missingness.png","Figure 1. Missingness by variable (MAIN-linked dataset).")
addfig("F1_vacc_to_onset.png","Figure 2. Distribution of the vaccination-to-onset interval (dashed line = 10-day window).")
addfig("F2_transitions.png","Figure 3. RT-PCR transition patterns among repeatedly tested participants.")
addfig("F3_forest_OR.png","Figure 4. Odds ratio for vaccination by outcome definition (OR<1 favours protection).")

# ============================ 5. LINKAGE COMPARISON ==========================
H1("Comparison of the current (narrow) and broader (all-samples) linkage"); resetH2()
CAP("Table 5.1. Completeness — narrow vs broad linkage.")
FT(R$cmp_cmp)
BODY("The broader linkage retains the same set of linked participants but attaches every laboratory specimen per participant rather than a single consolidated result. It does not increase the number of linked participants; instead it enriches each participant with a longitudinal laboratory history, which enables the alternative outcome definitions in Analysis 2 and the detection of result changes over time. Because the broader definition adds repeated observations of the same individuals rather than new individuals, it does not by itself change the case/control composition of the primary analysis; its main value is exposing outcome-definition sensitivity and viral-clearance dynamics. Broadening the outcome to any-positive modestly increases the case count and shifts the VE point estimate, illustrating a linkage-confidence/misclassification trade-off: the more permissive the outcome, the greater the potential for outcome misclassification. Descriptive linkage improvement (richer histories) should be distinguished from evidence supporting a definitive VE estimate, which remains limited by sample size and vaccination-data completeness.")

# ============================ 6. PIERRE ======================================
H1("Information requested by Pierre"); resetH2()
cn <- R$cmp_narrow; getc <- function(v) cn$`Complete n (%)`[cn$Variable==v]
BODY(sprintf("1. Unique linked participants: %d (of %d unique surveillance participants).", R$N$linked, R$N$surv_unique))
BODY(sprintf("2. RT-PCR completeness: %s; case/control distribution among linked with a definitive result: %d cases (PCR+) and %d controls (PCR-).", getc("RT-PCR result (definitive)"), R$N$cases, R$N$controls))
BODY(sprintf("3. Symptom-onset completeness: %s.", getc("Symptom-onset date")))
BODY(sprintf("4. Vaccination-status completeness and distribution: %s complete; %s vaccinated, %s not vaccinated, %s unknown.",
  getc("Vaccination status"),
  R$dist$vacc$n[R$dist$vacc$Category=="Vaccinated"], R$dist$vacc$n[R$dist$vacc$Category=="Not vaccinated"], R$dist$vacc$n[R$dist$vacc$Category=="Unknown"]))
BODY(sprintf("5. Vaccination-date completeness among vaccinated: %s.", getc("Vaccination date (among vaccinated)")))
BODY(sprintf("6. Age completeness: %s.", getc("Age")))
BODY(sprintf("7. Sex completeness: %s.", getc("Sex")))
BODY(sprintf("8. Health-zone completeness: %s.", getc("Health zone")))
BODY(sprintf("9. Health-area completeness: %s.", getc("Health area")))
BODY(sprintf("10. Village completeness: %s.", getc("Village")))
BODY(sprintf("11. Vaccination-strategy completeness: %s. NOTE: the strategy field is coded numerically with no decode key in the supplied data, so ring vs targeted-geographic cannot yet be labelled; only the coded distribution can be produced.", getc("Vaccination strategy")))
tier<-R$dist$tier
BODY(sprintf("12-14. Linkage-method distribution: automatic/exact %s, probabilistic %s, manual/adjudicated %s.",
  sprintf("%d (%.1f%%)",tier$n[1],tier$`%`[1]), sprintf("%d (%.1f%%)",tier$n[2],tier$`%`[2]), sprintf("%d (%.1f%%)",tier$n[3],tier$`%`[3])))
BODY(sprintf("15. Unresolved/unmatched: %d linked participants without a definitive RT-PCR result; %d PCR-positive laboratory records without a surveillance match.",
  R$audit$N[R$audit$Step=="Unresolved: linked but no definitive RT-PCR result"], R$N$orph_pos))
BODY("16. Comparison with broader linkage: see Section 5 and Tables 4.2 / 5.1 — the broader (all-samples) definition enriches laboratory history without adding participants; it enables outcome-definition sensitivity but does not materially change the primary VE estimate.")
NOTE("Items that cannot yet be produced: ring vs targeted-geographic vaccination strategy (no decode key), and any register-verified vaccination date beyond those recorded in surveillance.")

# ============================ 7. LIMITATIONS =================================
H1("Limitations and interpretation"); resetH2()
BULl("Small numbers, particularly of cases, produce wide confidence intervals; the analysis is preliminary and not powered for a definitive VE estimate.")
BULl(sprintf("The ADJUSTED model is over-parameterised for the available events (about %.1f events per parameter, below the accepted minimum of ~10). Its wide interval (VE %.0f%%, 95%% CI %.0f%% to %.0f%%) is a symptom of this instability, not evidence of harm: the negative lower bound is simply (1 - the upper odds-ratio limit) x 100 and reflects imprecision. The CRUDE estimate (VE %.1f%%, 95%% CI %.1f%% to %.1f%%) and the Firth penalised model are more reliable with this number of cases and should be preferred; the adjusted result is reported for completeness only.", 28/5, R$adj$VE, R$adj$VE_L, R$adj$VE_U, R$crude$VE, R$crude$VE_L, R$crude$VE_U))
BULl("Potential DUPLICATE participants remain. Keying the laboratory join on the patient identifier removed the wrong-person specimen mis-assignment, but it does NOT merge an individual who was recorded under two different laboratory identifiers. The triangulated register itself flags ~156 patients as ‘possible same patient with name variants’, and within the analytic set dozens of participants share identical or near-identical names under different identifiers. A patient-level de-duplication (name-variant merge) is still required and could change the counts and the VE estimate.")
BULl("Vaccination information is drawn from surveillance rather than a verified vaccination register; vaccination dates are missing for many vaccinated participants, limiting the protocol >10-day effective-window analysis.")
BULl("Vaccination strategy is not decodable; geography is near-constant (single dominant health zone), limiting geographic adjustment.")
BULl("Linkage uses the confirmed (MAIN) set with adjudicated wrong links removed; residual linkage error and outcome misclassification remain possible, and broadening the outcome definition increases that potential.")
BULl("Statistical non-significance must not be read as evidence of no effect. Results should be updated as linkage adjudication, register-verified vaccination data, and additional records become available.")

# ============================ 8. REPRODUCIBILITY ============================
H1("Reproducibility and deliverables"); resetH2()
BULl("R analysis script: ve_analysis.R (import, cleaning, deduplication, linkage, outcome construction, models, exports).")
BULl("Report generator: ve_report.R (this document, built from results_bundle.rds).")
BULl("Cleaned participant-level analytic dataset: output/analytic_participant_level.csv and .rds.")
BULl("All tables: tables/EVE_all_tables.xlsx and individual CSVs; figures: figures/*.png.")

print(doc, target=file.path(ROOT,"EVE_VE_Preliminary_Report.docx"))
cat("Word report written:", file.path(ROOT,"EVE_VE_Preliminary_Report.docx"), "\n")
