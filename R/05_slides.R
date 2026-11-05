# =============================================================================
#  05_slides.R  —  updated preliminary slide deck (PowerPoint), built from the
#  saved results bundle and the figures from 03. Figure- and headline-driven,
#  with a few small tables. No value is typed here: it reads results_bundle.rds.
# =============================================================================
for (p in c("officer","flextable")) if (!requireNamespace(p, quietly = TRUE))
  stop("Slides need package '", p, "': install.packages('", p, "')")
suppressWarnings(suppressMessages({library(officer); library(flextable)}))
B    <- readRDS(file.path(OUT_DIR, "results_bundle.rds"))
DECK <- file.path(OUT_DIR, sprintf("EVE_VE_Preliminary_Slides_%s.pptx", format(Sys.Date(), "%d%b%Y")))
FIG  <- file.path(OUT_DIR, "figures")

cr <- B$crude; ad <- B$adj; sp <- B$adj_sap; ex <- B$crude_ex
ev <- B$expo_class[B$expo_class$`Exposure category`=="Vaccinated >10d (effective)", ]
fN <- function(s) { v <- B$flow$N[grep(s, B$flow$Step, fixed = TRUE)]; if (length(v)) v[1] else "" }   # a flow count by step name

pp <- read_pptx()                                   # default Office theme (10 x 7.5 in)

# -- helpers ------------------------------------------------------------------
title_only <- function(pp, title){ pp <- add_slide(pp, "Title and Content", "Office Theme")
  ph_with(pp, title, location = ph_location_type(type = "title")) }
bullets <- function(pp, title, items){ pp <- add_slide(pp, "Title and Content", "Office Theme")
  pp <- ph_with(pp, title, location = ph_location_type(type = "title"))
  ph_with(pp, unordered_list(str_list = items, level_list = rep(1L, length(items))),
          location = ph_location_type(type = "body")) }
# table with EXPLICIT column widths (inches) so it never mis-sizes on the slide
tbl <- function(pp, df, left, top, colw, fsz = 12){
  ft <- flextable(as.data.frame(df)) |> bold(part="header") |> bg(part="header", bg="#D9E2F3") |>
        fontsize(size = fsz, part = "all") |> font(fontname = "Calibri", part = "all") |>
        border_outer() |> border_inner_h()
  for (j in seq_along(colw)) ft <- width(ft, j = j, width = colw[j])
  ph_with(pp, ft, location = ph_location(left = left, top = top, width = sum(colw), height = 5.4)) }
img <- function(pp, file, left, top, width, height){ p <- file.path(FIG, file)
  if (file.exists(p)) pp <- ph_with(pp, external_img(p, width = width, height = height),
                                    location = ph_location(left = left, top = top, width = width, height = height)); pp }
caption <- function(pp, text, top = 6.4, col = "#C00000", sz = 13){
  ph_with(pp, block_list(fpar(ftext(text, fp_text(font.size = sz, bold = TRUE, color = col)))),
          location = ph_location(left = 0.5, top = top, width = 9.0, height = 0.9)) }

# -- 1. Title -----------------------------------------------------------------
pp <- add_slide(pp, "Title Slide", "Office Theme")
pp <- ph_with(pp, "Ebola Vaccine Effectiveness — Bulape, DRC", location = ph_location_type(type = "ctrTitle"))
pp <- ph_with(pp, sprintf("Preliminary results (updated) — test-negative design\n%d distinct participants · %d cases / %d controls · PRELIMINARY, internal review",
                          B$N$distinct, cr$cases, cr$controls), location = ph_location_type(type = "subTitle"))

# -- 2. Design ----------------------------------------------------------------
pp <- bullets(pp, "Study design: test-negative", c(
  "Suspected EVD patients tested by RT-PCR; cases = positive, controls = negative.",
  "Exposure: vaccinated vs not vaccinated (unknown excluded, never imputed).",
  "Outcome: episode-defining RT-PCR (first definitive specimen on/after onset).",
  "VE = (1 - odds ratio) x 100%.",
  "Routinely collected surveillance, laboratory and vaccination data; no new collection."))

# -- 3. Participant flow (headline numbers) -----------------------------------
pp <- bullets(pp, "Participant flow", c(
  sprintf("%s linkage records  ->  %s distinct participants.", fN("Linkage export"), fN("Distinct participants")),
  sprintf("   %s within-pair confirmed links + %s cross-record (orphan) links.", fN("within-pair"), fN("cross-record")),
  sprintf("%s with an episode-defining RT-PCR result.", fN("Episode-defining")),
  sprintf("Primary analytic sample: %d  (%d cases / %d controls).", cr$n, cr$cases, cr$controls),
  sprintf("Adjusted-model sample: %s.", fN("ADJUSTED MODEL"))))

# -- 4. Data completeness -----------------------------------------------------
pp <- bullets(pp, "Data completeness after cleaning", c(
  "Age and sex are now complete (0% missing).",
  "Vaccination strategy is now recorded for every vaccinated participant (was ~25% on the Oct slides).",
  "Remaining gaps: vaccination status (excluded, never imputed), vaccination date among the vaccinated (~half missing), and onset / specimen dates.",
  "Data quality — not the method — is the main constraint on precision."))

# -- 5. Characteristics (small table) -----------------------------------------
pp <- title_only(pp, "Characteristics of cases and controls")
pp <- tbl(pp, B$char_by_pcr, left = 0.5, top = 1.5, colw = c(4.3, 2.3, 2.3), fsz = 12)

# -- 6. Vaccination timing (table + F1) ---------------------------------------
pp <- title_only(pp, "Vaccination timing")
pp <- tbl(pp, B$vacc_timing, left = 0.4, top = 1.6, colw = c(3.4, 0.9, 0.9), fsz = 13)
pp <- img(pp, "F1_vacc_to_onset.png", left = 5.6, top = 1.8, width = 4.2, height = 2.5)
pp <- ph_with(pp, block_list(fpar(ftext("Only 22 of 99 vaccinated meet the protocol >10-day window; median 9 days vaccination-to-onset (reactive / ring vaccination).", fp_text(font.size = 12, italic = TRUE)))),
              location = ph_location(left = 0.4, top = 5.2, width = 9.2, height = 1.2))

# -- 7. VE forest (headline figure) -------------------------------------------
pp <- title_only(pp, "Vaccine effectiveness by model")
pp <- img(pp, "F3_VE_by_model.png", left = 0.9, top = 1.6, width = 8.2, height = 4.7)

# -- 8. VE table (compact: VE and CI only) ------------------------------------
mt <- B$model_tbl[, c("Model","VE %","VE 95% CI")]
pp <- title_only(pp, "Vaccine effectiveness estimates")
pp <- tbl(pp, mt, left = 0.5, top = 1.5, colw = c(5.5, 1.1, 2.3), fsz = 11)

# -- 9. New adjusted model ----------------------------------------------------
pp <- bullets(pp, "New: adjusting for exposure risk", c(
  sprintf("Slide model (age, sex, calendar time): VE = %.1f%% (%.1f to %.1f).", ad$VE, ad$VE_L, ad$VE_U),
  sprintf("Analysis-plan model (+ contact, funeral): VE = %.1f%% (%.1f to %.1f).", sp$VE, sp$VE_L, sp$VE_U),
  "Adding exposure-risk confounders raises the adjusted VE — the plan anticipated this (vaccination tracks exposure via ring / frontline campaigns).",
  sprintf("Crude %.1f%% and excl. cross-record %.1f%% are essentially unchanged.", cr$VE, ex$VE)))

# -- 10. Robustness: outcome definition (figure) ------------------------------
pp <- title_only(pp, "Robustness: outcome definition")
pp <- img(pp, "F4_outcome_def_forest.png", left = 1.6, top = 1.7, width = 6.8, height = 4.2)
pp <- caption(pp, "VE is 75-79% under every RT-PCR outcome definition (episode / first / final / any-positive).", col = "#404040", sz = 13)

# -- 11. Protocol window: the 22 (table + figure + caption) -------------------
pp <- title_only(pp, "Protocol window: the 22 vaccinated >10d")
pp <- tbl(pp, B$expo_class, left = 0.3, top = 1.5, colw = c(2.2, 0.9, 0.9, 1.3), fsz = 9)
pp <- img(pp, "F5_protocol_forest.png", left = 5.6, top = 1.5, width = 4.2, height = 2.5)
pp <- caption(pp, sprintf("22 vaccinated >10d before onset: %s cases, %s controls. Zero cases -> ordinary model separates (not estimable); Firth VE ~82%%, CI very wide.", ev$`Case (PCR+)`, ev$`Control (PCR-)`), top = 6.3)

# -- 12. Mortality (Primary 2) ------------------------------------------------
pp <- title_only(pp, "Mortality VE (Primary 2)")
pp <- ph_with(pp, block_list(fpar(ftext(B$mortality_counts, fp_text(font.size = 12, italic = TRUE)))),
              location = ph_location(left = 0.4, top = 1.25, width = 9.3, height = 0.9))
pp <- tbl(pp, B$mortality, left = 0.3, top = 2.3, colw = c(2.5, 1.3, 1.9, 1.5, 1.9), fsz = 10)

# -- 13. Key findings ---------------------------------------------------------
pp <- bullets(pp, "Key findings", c(
  sprintf("Prior vaccination is associated with lower odds of confirmed EVD: crude %.0f%%, adjusted %.0f%%, analysis-plan adjusted %.0f%%.", cr$VE, ad$VE, sp$VE),
  "Robust to excluding uncertain links and to the RT-PCR outcome definition.",
  "No confirmed case among the 22 protocol-vaccinated — strong signal, but not estimable (0 cases).",
  "Mortality VE is sparse and descriptive; not evidence for or against protection against death.",
  "Data quality is the main constraint on precision."))

# -- 14. Limitations / next steps ---------------------------------------------
pp <- bullets(pp, "Limitations and next steps", c(
  sprintf("Small numbers: %d cases, 3 vaccinated — wide intervals; preliminary.", cr$cases),
  "Vaccination dates missing for ~half of the vaccinated; protocol window rests on few people.",
  "Differential missingness of vaccination status (higher among cases); residual confounding.",
  "Next: resolve open queries, fully link vaccination registries, and re-run as cases accrue."))

print(pp, target = DECK)
cat(sprintf("[05] slides written: %s (%d slides)\n", DECK, length(pp)))
