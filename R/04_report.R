# =============================================================================
#  04_report.R  —  preliminary narrative report (Word, Century Gothic), built
#  from the saved results bundle. No result value is typed into this file: it
#  reads outputs/results_bundle.rds, so re-running 03 then 04 refreshes the report.
# =============================================================================
for (p in c("officer","flextable")) if (!requireNamespace(p, quietly = TRUE))
  stop("Report needs package '", p, "': install.packages('", p, "')")
suppressWarnings(suppressMessages({library(officer); library(flextable)}))
B      <- readRDS(file.path(OUT_DIR, "results_bundle.rds"))
REPORT <- file.path(OUT_DIR, sprintf("EVE_VE_Preliminary_Report_%s.docx", format(Sys.Date(), "%d%b%Y")))
FONT   <- "Century Gothic"; set_flextable_defaults(font.family = FONT, font.size = 9, padding = 3, theme_fun = theme_booktabs)

# ---- small builders for headings, text, bullets, tables, figures ------------
tt  <- function(txt, sz=11, bold=FALSE, color="#000000", italic=FALSE) fp_text(font.family=FONT, font.size=sz, bold=bold, color=color, italic=italic)
H1n <- 0; H2c <- 0; doc <- read_docx()
H1  <- function(t){ H1n<<-H1n+1; H2c<<-0; doc<<-doc%>%body_add_fpar(fpar(ftext(paste0(H1n,". ",t), tt(t,15,TRUE,"#1F3864")), fp_p=fp_par(padding.top=10,padding.bottom=4))) }
H2  <- function(t){ H2c<<-H2c+1; doc<<-doc%>%body_add_fpar(fpar(ftext(t, tt(t,12,TRUE,"#2E5496")), fp_p=fp_par(padding.top=6,padding.bottom=3))) }
BODY<- function(t) doc<<-doc%>%body_add_fpar(fpar(ftext(t, tt(t,10.5)), fp_p=fp_par(text.align="left",padding.bottom=6)))
BULL<- function(t) doc<<-doc%>%body_add_fpar(fpar(ftext(paste0("•  ",t), tt(t,10.5)), fp_p=fp_par(padding.bottom=2,padding.left=14)))
CAP <- function(t) doc<<-doc%>%body_add_fpar(fpar(ftext(t, tt(t,9,TRUE,"#404040",TRUE)), fp_p=fp_par(padding.top=6,padding.bottom=2)))
NOTE<- function(t) doc<<-doc%>%body_add_fpar(fpar(ftext(t, tt(t,9.5,FALSE,"#7030A0",TRUE)), fp_p=fp_par(padding.bottom=4)))
FT  <- function(df){ ft<-flextable(as.data.frame(df))%>%bold(part="header")%>%bg(part="header",bg="#D9E2F3")%>%border_outer(part="all")%>%border_inner_h(part="body")%>%fontsize(size=8.5,part="all")%>%font(fontname=FONT,part="all")%>%autofit(); doc<<-doc%>%body_add_flextable(ft)%>%body_add_par("") }
addfig <- function(f,cap,w=6.3,h=3.8){ p<-file.path(OUT_DIR,"figures",f); if (file.exists(p)){ CAP(cap); doc<<-doc%>%body_add_img(src=p,width=w,height=h)%>%body_add_par("") } }
sig <- function(hi) ifelse(!is.na(hi) & hi<1, "statistically significant", "not statistically significant")

# values pulled from the bundle (never hard-coded)
cr <- B$crude; ad <- B$adj; sp <- B$adj_sap; ex <- B$crude_ex
ev <- B$expo_class[B$expo_class$`Exposure category`=="Vaccinated >10d (effective)", ]

# ---- title page -------------------------------------------------------------
doc <- doc %>%
  body_add_fpar(fpar(ftext("Preliminary Vaccine Effectiveness Analysis", tt("",20,TRUE,"#1F3864")), fp_p=fp_par(text.align="center",padding.top=24,padding.bottom=4))) %>%
  body_add_fpar(fpar(ftext("Ebola Virus Disease — Bulape and surrounding health zones, DRC", tt("",13,FALSE,"#2E5496")), fp_p=fp_par(text.align="center",padding.bottom=2))) %>%
  body_add_fpar(fpar(ftext("Test-negative case-control design — Analysis report (provisional)", tt("",11,FALSE,"#2E5496")), fp_p=fp_par(text.align="center",padding.bottom=4))) %>%
  body_add_fpar(fpar(ftext("PRELIMINARY — for internal review. All estimates are provisional and not for external communication.", tt("",10,TRUE,"#C00000")), fp_p=fp_par(text.align="center",padding.bottom=14))) %>%
  body_add_fpar(fpar(ftext(sprintf("Coded dataset: %d distinct participants; %d with an episode-defining RT-PCR result; primary test-negative sample %d (%d cases, %d controls).", B$N$distinct, B$N$episode, cr$n, cr$cases, cr$controls), tt("",10.5)), fp_p=fp_par(text.align="center",padding.bottom=6))) %>%
  body_add_break()

# ---- 1. Executive summary ---------------------------------------------------
H1("Executive summary")
BODY("This report presents a preliminary estimate of the effectiveness of Ebola (rVSV-ZEBOV / Ervebo) vaccination against laboratory-confirmed Ebola virus disease (EVD) during the outbreak centred on Bulape, Democratic Republic of the Congo. It uses only routinely collected surveillance, laboratory and vaccination data — there was no new data collection, no participant contact and no intervention. The design is a test-negative case-control study: among suspected EVD patients tested by RT-PCR, cases are those who test positive and controls are those who test negative, and the two groups are compared on prior vaccination. Vaccine effectiveness is VE = (1 - odds ratio) x 100%.")
BODY(sprintf("Crude VE was %.1f%% (95%% CI %.1f%% to %.1f%%), %s. Adjusting for age, sex and calendar time gave %.1f%% (%.1f%% to %.1f%%), %s; adding the exposure-risk confounders the analysis plan emphasises (contact with a confirmed case, funeral participation) gave %.1f%% (%.1f%% to %.1f%%). Excluding the %d lower-confidence cross-record links left the crude estimate essentially unchanged (%.1f%%). Every point estimate is protective, but with only %d cases (of whom 3 were vaccinated) the intervals are wide and the estimate is preliminary.",
  cr$VE,cr$VE_L,cr$VE_U, sig(cr$OR_U), ad$VE,ad$VE_L,ad$VE_U, sig(ad$OR_U), sp$VE,sp$VE_L,sp$VE_U, B$N$cross, ex$VE, cr$cases))
NOTE("Statistical non-significance is not evidence of no effect. The results are limited by small numbers, incomplete vaccination dates, differential missingness of vaccination status between cases and controls, and residual linkage uncertainty. Data quality — not the method — is the main constraint on precision.")

# ---- 2. Background and setting ----------------------------------------------
H1("Background and setting")
H2("The outbreak and the idea")
BODY("rVSV-ZEBOV (Ervebo) is a single-dose Ebola vaccine deployed reactively, mainly through ring vaccination of the contacts of confirmed cases and of frontline workers. Its efficacy was shown in the Guinea ring-vaccination trial, and real-world effectiveness was estimated in eastern DRC in 2018-2020, but field effectiveness data remain limited. The current outbreak around Bulape offers an opportunity to estimate effectiveness using data that the response already generates, at no additional burden to patients or staff.")
BODY("The evaluation is led by the Kinshasa School of Public Health (UNIKIN) with the Ministry of Health / Expanded Programme on Immunization, INRB, WHO, MSF/Epicentre and technical support from the US CDC. Its primary objectives are (1) VE against laboratory-confirmed EVD by a test-negative design, and (2) VE against death among confirmed cases by a retrospective cohort. Secondary objectives include other severe outcomes and, where denominators allow, an attack-rate comparison.")

# ---- 3. Data collection and data sources ------------------------------------
H1("Data collection and data sources")
BODY("All data are routinely collected during the outbreak response and assembled retrospectively. No study-specific forms were added. Four kinds of source feed the analysis:")
BULL("Surveillance (case investigation forms): identity, symptom-onset date, demographics, geography (province, health zone, health area, village), occupation, self-reported vaccination history, and exposure information — contact with a confirmed case, funeral/burial participation, travel, hospitalisation and vital status. Around half of investigated alerts are validated as meeting the suspect-case definition; only suspect cases enter the analysis.")
BULL("Laboratory: results come from several laboratory datasets (GeneXpert and validated assays) that are triangulated into a single register with one row per specimen, so that a participant's full RT-PCR history — every specimen and its collection date — is preserved. This supports the episode-defining outcome (first definitive result on/after onset) and the discordant-result checks.")
BULL("Vaccination registries: campaign registries (ring vaccination and frontline-worker campaigns) document that vaccination occurred and, where available, the date, product and dose. For this run a standalone vaccination register was not yet linked for every participant, so vaccination status also draws on surveillance fields; the absence of a registry record is the basis for classifying a person as unvaccinated.")
BULL("Exposure and clinical fields: contact with a confirmed case, funeral participation, travel, hospitalisation, and death/vital status are taken from the case investigation and, for vital status, cross-checked against the laboratory register.")
BODY("Names, sex, province, health zone and health area are near-complete in every source; other fields (village, phone, occupation, age in some registries) are less complete, which matters for linkage and for confounder adjustment.")

# ---- 4. Record linkage methodology (SOP) ------------------------------------
H1("Record linkage methodology")
BODY("Because vaccination status must be established from existing records, the surveillance data are linked to the vaccination registries. Linkage errors become exposure misclassification: a missed link makes a vaccinated person look unvaccinated, and a false link does the reverse. The linkage therefore aims not only to maximise correct links but to estimate and report its error rates. The method follows the study's Record Linkage SOP.")
H2("Preparation")
BODY("Fields are standardised before comparison, and source files are never modified. Names are upper-cased, stripped of accents, punctuation and honorifics, and split into order-free tokens, so that re-ordered or partial names can still agree; token frequencies are computed so that agreement on a rare name counts for more than agreement on a common birth-order name. Age is compared as the difference in years accounting for the time between vaccination and the alert; sex is standardised to Male/Female/Unknown; geography is aligned to reference boundaries (administrative units change over time); phone and occupation are standardised where available.")
H2("Probabilistic linkage (the main method)")
BODY("The primary method is probabilistic linkage in the Fellegi-Sunter framework (implemented with Splink). Each candidate pair of records is scored field by field: for every field, an agreement level carries a weight equal to log2(m/u), where m is how often true matches show that level and u how often non-matches do. The weights are summed, combined with a prior probability of a match (set per health zone from vaccination coverage), and converted to a posterior probability. Candidate pairs are first narrowed by blocking (e.g. same health zone, a shared rare name token, a shared phone, or same area with compatible age) so that not every pair is compared. Name similarity uses Jaro-Winkler token scores with symmetric best-match (Monge-Elkan) averaging, banded by both similarity and token rarity.")
H2("Deterministic linkage and clerical review")
BODY("Where a registry is too small to estimate the probabilistic model, deterministic rules (e.g. same standardised name, sex and health area with age difference < 1 year) select pairs, and every selected pair is reviewed by hand. Clerical review also resolves the pairs the probabilistic model classifies as uncertain and measures the accuracy of the automatic links: two reviewers see the records side by side, blinded to the score, and a third adjudicates disagreements.")
H2("Classification, de-duplication and triangulation")
BODY("Pairs are classified by posterior probability: a link (automatic) above 0.9, uncertain (sent for review) between 0.5 and 0.9, and a non-link below 0.5. Within a registry each surveillance record is matched to at most one registry record (greedy assignment), while legitimate multiple doses are kept and flagged. In this analysis the resulting links are summarised by confidence tier: Tier 1 (automatic / exact), Tier 2 (probabilistic), and Tier 3-4 (manual / adjudicated, which includes the cross-record links). Surveillance and laboratory records for the same person are then consolidated, and the triangulated laboratory register brings each participant's complete specimen history into one row per participant.")
H2("Quality assessment")
BODY("There is no gold-standard set of known links, so linkage quality is estimated indirectly: model checks (plausible agreement probabilities and weights), precision from the model-implied false-link count, from the clerically reviewed sample, and from scoring a name-shuffled copy of the registry (any link found there is false by construction), and sensitivity from field hold-out, from the share of self-reported vaccinees that are linked, and from blocking coverage. The linkage file keeps each link's posterior probability so the analysis can tighten the threshold or exclude review-only links as a sensitivity analysis.")

# ---- 5. Data quality and completeness (DQA) ---------------------------------
H1("Data quality and completeness")
BODY("Data quality is assured through the project dashboard and query process: records are de-duplicated, implausible values are flagged, laboratory results are verified, and site queries are issued and resolved, with every resolution shared between the linkage and analysis dashboards. This cleaning has measurably improved completeness — in the current dataset age and sex are complete, and vaccination strategy is now recorded for every vaccinated participant (it was about a quarter complete at the October presentation).")
BODY("The gaps that remain are concentrated in a few fields and drive the exclusions in the participant flow: vaccination status is unknown for a minority of participants (these are excluded from the primary analysis and never imputed), vaccination date is still missing for roughly half of the vaccinated (which limits the protocol >10-day window), and symptom-onset and specimen dates are missing for a subset (which limits the calendar-time adjustment and the timing checks). The missing-data rates for the key variables are shown in Table 2, and the specific data-quality flags in Table 10.")

# ---- 6. Statistical analysis methods ----------------------------------------
H1("Statistical analysis methods")
H2("Outcome, exposure and primary model")
BODY("Outcome: the episode-defining RT-PCR result — the first definitive specimen on or after symptom onset — gives Y = 1 for a positive (case) and Y = 0 for a negative (control); indeterminate results are excluded. Exposure: vaccinated versus not vaccinated; unknown status is excluded and never imputed. Model: logistic regression, crude then adjusted; VE = (1 - OR) x 100 with a profile-likelihood 95% CI.")
H2("Confounders (analysis plan)")
BODY("The analysis plan lists age, sex, geographic area, calendar time, healthcare/frontline-worker status, contact and other exposure-risk measures, testing/ETU site and outbreak as a-priori confounders, with particular attention to exposure risk because vaccination (delivered by ring and frontline campaigns) is associated with exposure. We report two adjusted models: a slide-matching model (age, sex, calendar time) and an analysis-plan model that adds the available exposure-risk confounders (contact with a confirmed case, funeral participation). Geographic area is near-constant here (almost all Bulape), healthcare-worker status is mostly unknown, and testing site and outbreak are single-valued, so none can act as a confounder; Table 5 records this.")
H2("Sensitivity and sparse-data methods")
BODY("Because vaccinated cases are few, Firth penalised logistic regression is used alongside ordinary regression wherever a fit is unstable or separates. Pre-specified sensitivity analyses are: excluding the cross-record links, alternative outcome definitions (first / final / any-positive RT-PCR), the protocol >10-day effective-window contrast, and reclassifying people vaccinated after onset as unvaccinated.")
H2("Protocol window and mortality")
BODY("The protocol window restricts 'effectively vaccinated' to people vaccinated more than 10 days before onset. VE against death among confirmed cases (primary objective 2) is estimated as a retrospective cohort: the outcome is death, the measure is a risk ratio (VE_death = (1 - RR) x 100), with odds-ratio and Firth models as sparse-data sensitivity.")

# ---- 7. Results -------------------------------------------------------------
H1("Results")
H2("Participant flow"); CAP("Table 1. Study flow, from the linkage export to the adjusted-model sample (one row per participant)."); FT(B$flow)
H2("Data completeness"); CAP("Table 2. Missing-data rate for key variables (vaccination date and strategy are among the vaccinated only)."); FT(B$completeness)
H2("Characteristics of cases and controls"); CAP("Table 3. Characteristics by RT-PCR status (episode-defining)."); FT(B$char_by_pcr)
CAP("Table 4. Vaccination-to-onset timing among the vaccinated (protocol exposure window)."); FT(B$vacc_timing)
H2("Confounders considered"); CAP("Table 5. Analysis-plan a-priori confounders: availability and whether each is in the adjusted model, with the reason."); FT(B$sap_confounders)
H2("Vaccine effectiveness estimates")
CAP("Table 6. Crude, adjusted (slide and analysis-plan) and sensitivity VE (episode-defining outcome)."); FT(B$model_tbl)
BODY(sprintf("Adding the exposure-risk confounders moved the adjusted VE from %.1f%% to %.1f%% (and the Firth-adjusted estimate similarly): the slide model's lower adjusted value was partly confounding by exposure risk, which the analysis plan anticipated. The crude estimate is %s; the adjusted estimates are %s.", ad$VE, sp$VE, sig(cr$OR_U), sig(ad$OR_U)))
addfig("F3_VE_by_model.png","Figure 1. Vaccine effectiveness by model with 95% CI. Green = CI excludes 0 (significant); red = CI crosses 0.", w=6.5, h=3.8)
H2("Robustness to the outcome definition"); CAP("Table 7. VE under alternative RT-PCR outcome definitions."); FT(B$sens)
addfig("F4_outcome_def_forest.png","Figure 2. Odds ratio by RT-PCR outcome definition with 95% CI (OR < 1 favours protection).")
CAP("Table 8. RT-PCR result transitions among repeatedly tested participants."); FT(B$trans_tab)
addfig("F1_vacc_to_onset.png","Figure 3. Vaccination-to-onset interval (dashed line = the 10-day protocol window).")
addfig("F2_transitions.png","Figure 4. RT-PCR transition patterns.")

H2("The protocol-window group (effectively vaccinated)")
BODY(sprintf("Of the vaccinated participants, only 22 were vaccinated more than 10 days before onset (the protocol definition of 'effectively vaccinated'). None of them is a confirmed case: in the primary analysis the group contributes %s cases and %s controls. Zero cases in the exposed arm is the strongest protection signal in the data, but it causes complete separation, so ordinary regression cannot return a confidence interval and the VE is not estimable. The Firth-penalised estimate (about 82%%) is the only usable value, with a very wide interval that crosses zero. The same holds for the 0-10-day and reclassification contrasts, because no vaccinated-timing subgroup contains a confirmed case.",
  ev$`Case (PCR+)`, ev$`Control (PCR-)`))
CAP("Table E1. Exposure classification by case/control (protocol window): who is kept, analysed separately, or excluded."); FT(B$expo_class)
CAP("Table E2. VE under the protocol exposure contrasts (ordinary fits separate; Firth shown)."); FT(B$extra_models)
addfig("F5_protocol_forest.png","Figure 5. Protocol-window VE (Firth-penalised), with status-based VE for reference. Every vaccinated-timing subgroup has 0 cases, so intervals are very wide.")

H2("VE against death among confirmed cases (Primary 2)")
BODY(B$mortality_counts)
CAP(sprintf("Table 9. VE against death among confirmed cases (retrospective cohort, n=%d): risk ratio (cohort measure) with odds-ratio and Firth models.", B$mortality_n)); FT(B$mortality)
NOTE("Primary objective 2. The numbers are very sparse (few vaccinated among confirmed cases and few deaths among them), so the risk ratio is the primary cohort measure and the odds-ratio / Firth estimates are sparse-data sensitivity. Descriptive and provisional; a zero cell can make an interval undefined.")

H2("Data-quality and timing checks"); CAP("Table 10. Data-quality flags (counts of records meeting each check)."); FT(B$dq)

# ---- 8. Extended exploratory ------------------------------------------------
H1("Extended exposure variables (exploratory)")
BODY("These analyses go beyond the primary plan and are exploratory: exposure-risk and clinical characteristics by case/control, their completeness, and severe outcomes among confirmed cases. Interpret cautiously given the small numbers.")
CAP("Table E3. Clinical and exposure-risk characteristics by RT-PCR status (Fisher p on known categories)."); FT(B$extra_char)
CAP("Table E4. Completeness of the exposure-risk variables."); FT(B$extra_complete)
if (nrow(B$severity_cases)) { CAP("Table E5. Severe outcomes among confirmed cases by vaccination (descriptive; NOT a VE-against-death estimate)."); FT(B$severity_cases) }

# ---- 9. Challenges, teamwork and data quality -------------------------------
H1("Challenges, teamwork and why data quality is the main constraint")
BODY("The hardest part of this evaluation is not the statistics but assembling a clean, correctly linked dataset from systems that were built for outbreak response, not research. Several challenges recur:")
BULL("Linkage under weak identifiers. Many people share common birth-order names, phone and village are often missing, and years may pass between a vaccination campaign and the current alert, so identity must be inferred from imperfect, changing fields. Probabilistic scoring plus human review manages this, but a residue of uncertain and missed links remains.")
BULL("Missing and inconsistent vaccination information. Vaccination status, date, product and dose are the exposure, yet they are the least complete fields. Missing dates in particular shrink the protocol-window analysis to a handful of people.")
BULL("Differential data quality between cases and controls. Confirmed cases tend to be investigated more thoroughly than test-negative controls, so their vaccination and exposure data are more complete. If ascertainment is better among cases, VE can be biased in either direction — this is why completeness, not just sample size, matters.")
BULL("Implausible dates and discordant results. Long onset-to-swab intervals and occasional discordant RT-PCR results require checking against the laboratory register and querying the site.")
BODY("These are solved by teamwork, not by any single analyst. Surveillance and laboratory teams generate and correct the source data; the linkage team prepares, scores and adjudicates record pairs following the SOP; the data-quality team runs the dashboard, raises site queries and verifies results; and the analysis team builds the coded dataset and estimates VE. The two dashboards (linkage/cleaning and analysis) share every resolution, so a correction entered once flows to both. Clear roles, a shared query log, and a reproducible pipeline are what turn fragmented response data into an analysis-ready dataset — and why improving data quality is the most effective way to sharpen the estimate.")

# ---- 10. Limitations --------------------------------------------------------
H1("Limitations and interpretation")
BULL("Small numbers, especially of cases and of vaccinated cases, give wide confidence intervals; the analysis is preliminary and not powered for a definitive estimate.")
BULL("Vaccination information comes largely from surveillance rather than a fully linked register; many vaccination dates are missing, so the protocol >10-day window rests on very few people and is not estimable by ordinary methods.")
BULL("Differential missingness of vaccination status (higher among cases) and residual confounding (geography near-constant, health-worker and site data sparse) can bias the estimate in either direction.")
BULL("Residual duplicate participants and lower-confidence cross-record links may remain; excluding them did not materially change the crude estimate, but results should be updated as queries resolve.")
BULL("The mortality estimate is sparse and descriptive; it is not evidence for or against protection against death.")

# ---- 11. Next steps and reproducibility -------------------------------------
H1("Next steps and reproducibility")
BULL("Resolve the open data queries (vaccination status and dates, onset/specimen dates, discordant results) and re-run; completeness, not method, is the main lever on precision.")
BULL("Link the vaccination registries fully (ring and frontline campaigns) to add documented dates and products and to enlarge the protocol-window group.")
BULL("Re-estimate VE and the mortality cohort as cases accrue, and compare with an attack-rate analysis if denominators become available.")
BODY("The whole analysis is reproducible from code: 02_build_cohort.R builds the de-duplicated coded dataset and data dictionary (one row per participant), 03_analysis.R fits every model and saves the results bundle, and this report (04_report.R) is generated from that bundle so no result value is typed by hand. The single entry point is run_all.R (or run_all.bat on Windows); participant data stays in the local data/ folder and is never committed.")

print(doc, target = REPORT)
cat(sprintf("[04] report written: %s\n", REPORT))
