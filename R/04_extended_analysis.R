# =============================================================================
#  04_extended_analysis.R  —  analyses requested for the final report.
#
#  Reads : Data/EVE_VE_analysis_dataset_deidentified.csv (via 03's D)
#  Writes: Outputs/extended_tables.xlsx   (every table, one per sheet)
#          Outputs/extended_bundle.rds    (all results)
#          Outputs/figures/F6..F13.png
#  Everything written here is aggregate (counts, percentages, estimates).
#
#  Sections
#    1  Set-up: analysis variables and model helpers (Firth via logistf)
#    2  Table 1: characteristics of cases and controls
#    3  Table 2: characteristics by vaccination status (confounding check)
#    4  Vaccination details among vaccinated participants
#    5  Vaccine effectiveness: one table across all methods
#    6  Propensity-score (IPTW) analysis + covariate balance
#    7  Latent class model of exposure risk + VE adjusted for latent class
#    8  Subgroup analyses with interaction tests
#    9  Death and severe disease
#    10 Co-infection (malaria)
#    11 Epidemic curve data
#    12 Figures
#    13 Save
# =============================================================================
if (!exists("D") || !"V_status" %in% names(D)) source(file.path(SCRIPTS, "03_main_analysis.R"), local = FALSE)
suppressWarnings(suppressMessages({ library(logistf); library(sandwich); library(tidyr); library(survival) }))
set.seed(20261118)

# ---- 1. Set-up ---------------------------------------------------------------
D$burial   <- ifelse(D$burial_attended %in% c("Yes","No"), D$burial_attended, "Unknown")
D$animal   <- yn(D$animal_contact); D$healer <- yn(D$traditional_healer); D$followed <- yn(D$contact_followed)
D$hemo     <- yn(D$hemorrhage); D$preg <- yn(D$pregnant)
D$sdead    <- ifelse(D$sampled_dead == "Yes", "Yes", "No")
D$malaria  <- ifelse(D$malaria_detected == "Yes", "Yes", "No")
D$ct_gp    <- numv(D$ct_gp_min)
D$zone2    <- ifelse(D$health_zone == "Bulape", "Bulape", ifelse(nzchar(D$health_zone), "Other zone", "Unknown"))
D$tier2    <- ifelse(grepl("Tier 1", D$linkage_tier), "Tier 1 (exact)", ifelse(grepl("Tier 2", D$linkage_tier), "Tier 2 (token)", "Tier 3-4 / cross-record"))
D$agegrp3  <- cut(D$age, c(-Inf, 17, 44, Inf), labels = c("0-17","18-44","45+"))
D$age_lbl  <- as.character(D$agegrp); D$age_lbl[is.na(D$age_lbl)] <- "Unknown"
D$sex_lbl  <- ifelse(D$sex %in% c("Male","Female"), D$sex, "Unknown")
D$hcw2     <- ifelse(D$hcw == "Yes", "Yes", "No/unknown")
VAX_START  <- as.Date("2025-09-13")                                  # vaccination campaign start (dashboard)
D$period   <- ifelse(is.na(D$onset), "Unknown", ifelse(D$onset < VAX_START, "Onset before 13 Sep 2025", "Onset from 13 Sep 2025"))
D$fortnight <- ifelse(is.na(D$onset), NA, format(as.Date(cut(D$onset, "2 weeks", start.on.monday = TRUE)), "%Y-%m-%d"))
D$onset_wk <- ifelse(is.na(D$onset), NA, format(D$onset, "%G-W%V"))
D$died_bin <- ifelse(D$died == "Yes", 1L, ifelse(D$died == "No", 0L, NA_integer_))
cc <- D[!is.na(D$Y_episode), ]; cc$grp <- ifelse(cc$Y_episode == 1, "Cases", "Controls")
PS <- D[!is.na(D$Y_episode) & !is.na(D$V_status), ]                  # primary analytic sample

pf  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
f1  <- function(x) ifelse(is.na(x) | !is.finite(x), "NA", sprintf("%.1f", x))
f2  <- function(x) ifelse(is.na(x) | !is.finite(x), "NA", sprintf("%.2f", x))

# Firth logistic regression with profile-likelihood CI (logistf). Returns exposure OR/VE.
ffit <- function(dat, outcome, exposure, adj = character(0)) {
  dat <- dat[!is.na(dat[[outcome]]) & !is.na(dat[[exposure]]), , drop = FALSE]
  adj <- adj[vapply(adj, function(a) length(unique(na.omit(dat[[a]]))) > 1, logical(1))]
  if (length(adj)) dat <- dat[complete.cases(dat[, adj, drop = FALSE]), , drop = FALSE]
  cells <- c(vc = sum(dat[[outcome]] == 1 & dat[[exposure]] == 1), uc = sum(dat[[outcome]] == 1 & dat[[exposure]] == 0),
             vk = sum(dat[[outcome]] == 0 & dat[[exposure]] == 1), uk = sum(dat[[outcome]] == 0 & dat[[exposure]] == 0))
  out <- list(n = nrow(dat), cells = cells, OR = NA, L = NA, U = NA, p = NA, adj = adj)
  if (nrow(dat) < 5 || sum(dat[[exposure]]) == 0 || sum(dat[[exposure]] == 0) == 0 || sum(dat[[outcome]]) == 0) return(out)
  fm <- as.formula(paste(outcome, "~", paste(c(exposure, adj), collapse = " + ")))
  m <- tryCatch(suppressWarnings(logistf(fm, data = dat, pl = TRUE)), error = function(e) NULL)
  if (is.null(m)) return(out)
  j <- which(names(coef(m)) == exposure)
  out$OR <- exp(coef(m)[j]); out$L <- exp(m$ci.lower[j]); out$U <- exp(m$ci.upper[j]); out$p <- m$prob[j]; out$model <- m
  out }
# ordinary logistic with profile CI (NA when a zero cell makes it unestimable)
gfit <- function(dat, outcome, exposure, adj = character(0)) {
  r <- ffit(dat, outcome, exposure, adj); r$OR <- r$L <- r$U <- NA
  if (any(r$cells == 0) || r$n < 5) return(r)
  dat <- dat[!is.na(dat[[outcome]]) & !is.na(dat[[exposure]]), , drop = FALSE]
  if (length(r$adj)) dat <- dat[complete.cases(dat[, r$adj, drop = FALSE]), , drop = FALSE]
  m <- tryCatch(glm(as.formula(paste(outcome, "~", paste(c(exposure, r$adj), collapse = " + "))), data = dat, family = binomial()), error = function(e) NULL)
  if (is.null(m)) return(r)
  ci <- suppressMessages(tryCatch(confint(m), error = function(e) confint.default(m)))
  r$OR <- exp(coef(m)[exposure]); r$L <- exp(ci[exposure, 1]); r$U <- exp(ci[exposure, 2]); r }
# exact conditional (Fisher) OR for a 2x2
xfit <- function(dat, outcome, exposure) {
  r <- ffit(dat, outcome, exposure); r$OR <- r$L <- r$U <- NA; cl <- r$cells
  ft <- tryCatch(fisher.test(matrix(c(cl["vc"], cl["vk"], cl["uc"], cl["uk"]), 2)), error = function(e) NULL)
  if (!is.null(ft)) { r$OR <- unname(ft$estimate); r$L <- ft$conf.int[1]; r$U <- ft$conf.int[2]; r$p <- ft$p.value }
  r }
# conditional logistic regression, matched on a stratum (e.g. fortnight of onset)
clfit <- function(dat, outcome, exposure, stratum) {
  dat <- dat[!is.na(dat[[outcome]]) & !is.na(dat[[exposure]]) & !is.na(dat[[stratum]]), , drop = FALSE]
  inf <- tapply(dat[[outcome]], dat[[stratum]], function(y) length(unique(y)) == 2)            # strata with cases and controls
  dat <- dat[dat[[stratum]] %in% names(inf)[inf], , drop = FALSE]
  r <- ffit(dat, outcome, exposure); r$OR <- r$L <- r$U <- NA
  m <- tryCatch(survival::clogit(as.formula(paste(outcome, "~", exposure, "+ strata(", stratum, ")")), data = dat), error = function(e) NULL)
  if (!is.null(m) && all(r$cells > 0)) { b <- coef(m)[1]; se <- sqrt(vcov(m)[1, 1]); r$OR <- exp(b); r$L <- exp(b - 1.96 * se); r$U <- exp(b + 1.96 * se) }
  r }
# one row of the classic VE table
verow <- function(section, label, method, r) {
  vc <- r$cells["vc"]; nc <- r$cells["vc"] + r$cells["uc"]; vk <- r$cells["vk"]; nk <- r$cells["vk"] + r$cells["uk"]
  ve <- (1 - c(r$OR, r$U, r$L)) * 100
  data.frame(Section = section, Analysis = label, Method = method,
    `Vaccinated / cases` = sprintf("%d / %d (%.1f%%)", vc, nc, ifelse(nc > 0, 100 * vc / nc, 0)),
    `Vaccinated / controls` = sprintf("%d / %d (%.1f%%)", vk, nk, ifelse(nk > 0, 100 * vk / nk, 0)),
    `OR (95% CI)` = ifelse(is.na(r$OR), "not estimable", sprintf("%s (%s to %s)", f2(r$OR), f2(r$L), f2(r$U))),
    `VE % (95% CI)` = ifelse(is.na(r$OR), "not estimable", sprintf("%s (%s to %s)", f1(ve[1]), f1(ve[2]), f1(ve[3]))),
    VE = ve[1], VE_L = ve[2], VE_U = ve[3], N = r$n, check.names = FALSE, row.names = NULL) }

# ---- 2/3. Characteristics tables --------------------------------------------
# cat_rows: n (column %) per group; p from Fisher's exact test on known categories
cat_rows <- function(dat, gvar, var, label, levels = NULL, drop_unknown = c("Unknown")) {
  x <- as.character(dat[[var]]); x[is.na(x) | x == ""] <- "Unknown"; G <- sort(unique(dat[[gvar]]))
  if (is.null(levels)) levels <- c(setdiff(names(sort(table(x), decreasing = TRUE)), "Unknown"), if (any(x == "Unknown")) "Unknown")
  k <- !(x %in% drop_unknown); tb <- table(factor(x[k], levels = setdiff(levels, drop_unknown)), dat[[gvar]][k])
  tb <- tb[rowSums(tb) > 0, , drop = FALSE]
  p <- if (nrow(tb) >= 2 && ncol(tb) >= 2) tryCatch(fisher.test(tb, workspace = 2e7)$p.value,
         error = function(e) tryCatch(fisher.test(tb, simulate.p.value = TRUE, B = 2e4)$p.value, error = function(e) NA)) else NA
  out <- data.frame(Characteristic = c(label, paste0("   ", levels)), check.names = FALSE)
  for (g in G) { d <- x[dat[[gvar]] == g]; t <- as.integer(table(factor(d, levels = levels)))
    out[[g]] <- c("", sprintf("%d (%.1f)", t, 100 * t / max(1, length(d)))) }
  out$`p-value` <- c(pf(p), rep("", length(levels))); out }
num_rows <- function(dat, gvar, var, label) {
  x <- dat[[var]]; G <- sort(unique(dat[[gvar]]))
  p <- tryCatch(wilcox.test(x ~ dat[[gvar]], exact = FALSE)$p.value, error = function(e) NA)
  out <- data.frame(Characteristic = paste0(label, ", median (IQR)"), check.names = FALSE)
  for (g in G) { d <- x[dat[[gvar]] == g & !is.na(x)]
    out[[g]] <- if (length(d)) sprintf("%.0f (%.0f-%.0f)", median(d), quantile(d, .25), quantile(d, .75)) else "-" }
  out$`p-value` <- pf(p); out }
hdr_row <- function(dat, gvar, label) { G <- sort(unique(dat[[gvar]])); o <- data.frame(Characteristic = label, check.names = FALSE)
  for (g in G) o[[g]] <- ""; o$`p-value` <- ""; o }
n_row <- function(dat, gvar) { G <- sort(unique(dat[[gvar]])); o <- data.frame(Characteristic = "Participants, n", check.names = FALSE)
  for (g in G) o[[g]] <- as.character(sum(dat[[gvar]] == g)); o$`p-value` <- ""; o }

char_table <- function(dat, gvar) rbind(
  n_row(dat, gvar),
  hdr_row(dat, gvar, "Demographics"),
  num_rows(dat, gvar, "age", "Age (years)"),
  cat_rows(dat, gvar, "age_lbl", "Age group (years)", c(AGE_LABELS, "Unknown")),
  cat_rows(dat, gvar, "sex_lbl", "Sex", c("Female","Male","Unknown")),
  cat_rows(dat, gvar, "zone2", "Health zone of residence", c("Bulape","Other zone","Unknown")),
  cat_rows(dat, gvar, "hcw2", "Health or frontline worker", c("Yes","No/unknown"), drop_unknown = character(0)),
  hdr_row(dat, gvar, "Vaccination"),
  cat_rows(dat, gvar, "vacc_status", "Vaccination status", c("Vaccinated","Not vaccinated","Unknown")),
  cat_rows(dat, gvar, "exposure", "Timing of vaccination (protocol)", expo_lv, drop_unknown = "Unknown status"),
  hdr_row(dat, gvar, "Exposure history (21 days before onset)"),
  cat_rows(dat, gvar, "contact_c", "Contact with a confirmed or suspected case", c("Yes","No","Other","Unknown")),
  cat_rows(dat, gvar, "contact_k", "Known contact on a contact list", c("Yes","No","Other","Unknown")),
  cat_rows(dat, gvar, "funeral", "Took part in a funeral", c("Yes","No","Unknown")),
  cat_rows(dat, gvar, "burial", "Attended a burial", c("Yes","No","Unknown")),
  cat_rows(dat, gvar, "travel", "Travelled", c("Yes","No","Unknown")),
  cat_rows(dat, gvar, "animal", "Contact with animals", c("Yes","No","Unknown")),
  cat_rows(dat, gvar, "healer", "Visited a traditional healer", c("Yes","No","Unknown")),
  hdr_row(dat, gvar, "Clinical course"),
  cat_rows(dat, gvar, "hosp", "Hospitalised", c("Yes","No","Unknown")),
  cat_rows(dat, gvar, "hemo", "Haemorrhagic signs", c("Yes","No","Unknown")),
  cat_rows(dat, gvar, "sdead", "Specimen taken after death", c("Yes","No"), drop_unknown = character(0)),
  cat_rows(dat, gvar, "died", "Died", c("Yes","No","Unknown")),
  cat_rows(dat, gvar, "malaria", "Malaria recorded by the laboratory", c("Yes","No"), drop_unknown = character(0)),
  hdr_row(dat, gvar, "Linkage"),
  cat_rows(dat, gvar, "tier2", "Linkage tier", c("Tier 1 (exact)","Tier 2 (token)","Tier 3-4 / cross-record"), drop_unknown = character(0)))

table1 <- char_table(cc, "grp")
t2dat  <- PS; t2dat$vgrp <- ifelse(t2dat$V_status == 1, "Vaccinated", "Not vaccinated")
table2 <- char_table(t2dat, "vgrp")
fem <- cc[cc$sex == "Female", ]; preg_tab <- if (nrow(fem)) cat_rows(fem, "grp", "preg", "Pregnant (women only)", c("Yes","No","Unknown")) else data.frame()

# ---- 4. Vaccination details among vaccinated ---------------------------------
VV <- D[D$vacc_status == "Vaccinated", ]; VV$grp <- ifelse(VV$Y_episode %in% 1, "Cases", ifelse(VV$Y_episode %in% 0, "Controls", "No definitive result"))
VV$vprod <- ifelse(nzchar(VV$vacc_product), VV$vacc_product, "Not recorded")
VV$vcard2 <- ifelse(VV$vcard %in% c("Yes","No"), VV$vcard, "Unknown")
VV$vstrat <- ifelse(nzchar(VV$vacc_strategy_code), paste("Code", VV$vacc_strategy_code), "Not recorded")
VV$vsrc <- ifelse(nzchar(VV$vacc_source_code), paste("Code", VV$vacc_source_code), "Not recorded")
VV$dose <- ifelse(nzchar(VV$n_doses), paste(VV$n_doses, "dose date(s)"), "No date")
VV$all <- "All vaccinated"
vacc_details <- rbind(
  data.frame(Characteristic = "Vaccinated participants, n", `All vaccinated` = as.character(nrow(VV)), check.names = FALSE),
  do.call(rbind, lapply(list(c("vprod","Vaccine product"), c("vcard2","Vaccination card seen"), c("vsrc","Source of information (code)"),
                             c("vstrat","Vaccination strategy (code, no key)"), c("dose","Doses with a recorded date"), c("exposure","Timing relative to onset")),
    function(v) { r <- cat_rows(VV, "all", v[1], v[2], drop_unknown = character(0)); r[, c("Characteristic","All vaccinated")] })),
  data.frame(Characteristic = "Days from vaccination to onset, median (IQR)", `All vaccinated` = {
    x <- VV$vax_to_onset[!is.na(VV$vax_to_onset)]; if (length(x)) sprintf("%.0f (%.0f to %.0f), n=%d", median(x), quantile(x, .25), quantile(x, .75), length(x)) else "-" },
    check.names = FALSE))

# ---- 5/6/7 need the PS and LCA results; compute those first ----------------
## 6. Propensity score (IPTW, stabilised, truncated at 1st/99th percentiles)
PSV <- c("agegrp","sex_f","caltime","contact_adj","funeral_adj")
ipd <- PS[complete.cases(PS[, PSV]), ]
psm <- glm(V_status ~ agegrp + sex_f + caltime + contact_adj + funeral_adj, data = ipd, family = binomial())
ipd$ps <- fitted(psm); pv <- mean(ipd$V_status)
ipd$w <- ifelse(ipd$V_status == 1, pv / ipd$ps, (1 - pv) / (1 - ipd$ps))
q <- quantile(ipd$w, c(.01, .99)); ipd$w <- pmin(pmax(ipd$w, q[1]), q[2])
iptw <- local({ r <- ffit(ipd, "Y_episode", "V_status"); r$OR <- r$L <- r$U <- NA
  m <- tryCatch(suppressWarnings(glm(Y_episode ~ V_status, data = ipd, family = quasibinomial(), weights = w)), error = function(e) NULL)
  if (!is.null(m) && all(r$cells > 0)) { se <- sqrt(sandwich::vcovHC(m, type = "HC0")["V_status","V_status"]); b <- coef(m)["V_status"]
    r$OR <- exp(b); r$L <- exp(b - 1.96 * se); r$U <- exp(b + 1.96 * se) }
  r })
smd <- function(x, t, w = NULL) { if (is.null(w)) w <- rep(1, length(x)); m1 <- weighted.mean(x[t == 1], w[t == 1]); m0 <- weighted.mean(x[t == 0], w[t == 0])
  v1 <- sum(w[t == 1] * (x[t == 1] - m1)^2) / sum(w[t == 1]); v0 <- sum(w[t == 0] * (x[t == 0] - m0)^2) / sum(w[t == 0])
  (m1 - m0) / sqrt((v1 + v0) / 2) }
bal_vars <- model.matrix(~ agegrp + sex_f + caltime + contact_adj + funeral_adj, ipd)[, -1, drop = FALSE]
balance <- data.frame(Covariate = colnames(bal_vars),
  `SMD before weighting` = round(apply(bal_vars, 2, smd, t = ipd$V_status), 3),
  `SMD after weighting` = round(apply(bal_vars, 2, smd, t = ipd$V_status, w = ipd$w), 3), check.names = FALSE, row.names = NULL)
balance$Covariate <- sub("^agegrp", "Age group ", sub("^sex_f", "Sex ", sub("^contact_adj", "Contact with case: ", sub("^funeral_adj", "Funeral: ", sub("caltime", "Calendar time (weeks)", balance$Covariate)))))

## 7. Latent class model of exposure risk (binary indicators, EM, missing allowed)
LCI <- c(contact_c = "Contact with a case", contact_k = "On a contact list", funeral = "Took part in a funeral",
         burial = "Attended a burial", travel = "Travelled", animal = "Contact with animals", healer = "Visited a traditional healer")
Y <- sapply(names(LCI), function(v) ifelse(D[[v]] == "Yes", 1, ifelse(D[[v]] %in% c("No","Other"), 0, NA)))
lca_em <- function(Y, K, starts = 30, maxit = 2000, tol = 1e-8) {
  n <- nrow(Y); J <- ncol(Y); obs <- !is.na(Y); Y0 <- Y; Y0[!obs] <- 0; best <- NULL
  for (s in seq_len(starts)) {
    pi_k <- rep(1 / K, K); th <- matrix(runif(K * J, .1, .9), K, J); ll_old <- -Inf
    for (it in seq_len(maxit)) {
      lp <- sapply(seq_len(K), function(k) log(pi_k[k]) + rowSums(obs * (Y0 * log(rep(th[k, ], each = n)) + (1 - Y0) * log(1 - rep(th[k, ], each = n)))))
      lp <- matrix(lp, n, K); mx <- apply(lp, 1, max); post <- exp(lp - mx); rs <- rowSums(post); post <- post / rs
      ll <- sum(mx + log(rs))
      pi_k <- colMeans(post)
      th <- t(sapply(seq_len(K), function(k) colSums(post[, k] * Y0 * obs) / pmax(colSums(post[, k] * obs), 1e-10)))
      if (K == 1) th <- matrix(th, 1, J); th <- pmin(pmax(th, 1e-4), 1 - 1e-4)
      if (abs(ll - ll_old) < tol) break; ll_old <- ll }
    if (is.null(best) || ll > best$ll) best <- list(ll = ll, pi = pi_k, theta = th, post = post, K = K) }
  npar <- (K - 1) + K * J; best$bic <- -2 * best$ll + npar * log(n); best$aic <- -2 * best$ll + 2 * npar
  best$entropy <- if (K > 1) 1 - sum(-best$post * log(pmax(best$post, 1e-12))) / (n * log(K)) else NA
  best }
keepY <- rowSums(!is.na(Y)) >= 3                                        # need >=3 answered items
lca_fits <- lapply(1:4, function(K) lca_em(Y[keepY, , drop = FALSE], K))
lca_fit_tbl <- data.frame(Classes = 1:4, `Log-likelihood` = round(sapply(lca_fits, `[[`, "ll"), 1),
  BIC = round(sapply(lca_fits, `[[`, "bic"), 1), AIC = round(sapply(lca_fits, `[[`, "aic"), 1),
  Entropy = round(sapply(lca_fits, `[[`, "entropy"), 2),
  `Smallest class (%)` = round(100 * sapply(lca_fits, function(f) min(f$pi)), 1), check.names = FALSE)
ok <- which(lca_fit_tbl$`Smallest class (%)` >= 5)
Kbest <- ok[which.min(lca_fit_tbl$BIC[ok])]
LC <- lca_fits[[Kbest]]
ordc <- order(rowMeans(LC$theta))                                       # class 1 = lowest exposure
LC$theta <- LC$theta[ordc, , drop = FALSE]; LC$pi <- LC$pi[ordc]; LC$post <- LC$post[, ordc, drop = FALSE]
cls_names <- if (Kbest == 1) "Single class" else if (Kbest == 2) c("Lower exposure risk","Higher exposure risk") else
  paste0(c("Lower","Intermediate","Higher","Highest")[seq_len(Kbest)], " exposure risk")
D$lclass <- "Not classified (<3 items answered)"
D$lclass[keepY] <- cls_names[max.col(LC$post, ties.method = "first")]
lca_profile <- data.frame(Indicator = unname(LCI), round(100 * t(LC$theta), 1), check.names = FALSE)
names(lca_profile)[-1] <- paste0(cls_names, " (", round(100 * LC$pi), "%)")
PS <- D[!is.na(D$Y_episode) & !is.na(D$V_status), ]                  # refresh with lclass
PS$lclass_f <- factor(PS$lclass); PS$lclass_hi <- as.integer(PS$lclass == cls_names[length(cls_names)])
cc <- D[!is.na(D$Y_episode), ]; cc$grp <- ifelse(cc$Y_episode == 1, "Cases", "Controls")
lca_by_status <- cat_rows(cc, "grp", "lclass", "Latent exposure class", c(cls_names, "Not classified (<3 items answered)"),
                          drop_unknown = "Not classified (<3 items answered)")

# ---- 5. VE across all methods (one classic table) ----------------------------
A1 <- c("agegrp","sex_f","caltime"); A2 <- c(A1, "contact_adj","funeral_adj")
Dwp <- D[D$is_cross_record != "Yes", ]
ve_table <- rbind(
  verow("A. Any recorded vaccination (Report 001 definition)", "Crude", "Logistic regression", gfit(D, "Y_episode", "V_status")),
  verow("A. Any recorded vaccination (Report 001 definition)", "Crude", "Firth penalised", ffit(D, "Y_episode", "V_status")),
  verow("A. Any recorded vaccination (Report 001 definition)", "Crude", "Exact conditional", xfit(D, "Y_episode", "V_status")),
  verow("A. Any recorded vaccination (Report 001 definition)", "Adjusted: age, sex, calendar time", "Firth penalised", ffit(D, "Y_episode", "V_status", A1)),
  verow("A. Any recorded vaccination (Report 001 definition)", "Adjusted: + contact and funeral", "Firth penalised", ffit(D, "Y_episode", "V_status", A2)),
  verow("A. Any recorded vaccination (Report 001 definition)", "Adjusted: age, sex, calendar time + latent exposure class", "Firth penalised", ffit(PS, "Y_episode", "V_status", c(A1, "lclass_f"))),
  verow("A. Any recorded vaccination (Report 001 definition)", "Inverse probability of treatment weighting", "Weighted logistic, robust SE", iptw),
  verow("A. Any recorded vaccination (Report 001 definition)", "Matched on fortnight of onset", "Conditional logistic", clfit(PS, "Y_episode", "V_status", "fortnight")),
  verow("A. Any recorded vaccination (Report 001 definition)", "Onset on or after 13 Sep 2025 (vaccination period)", "Firth penalised", ffit(D[!is.na(D$onset) & D$onset >= VAX_START, ], "Y_episode", "V_status")),
  verow("A. Any recorded vaccination (Report 001 definition)", "Onset on or after 13 Sep 2025, adjusted age, sex, calendar time", "Firth penalised", ffit(D[!is.na(D$onset) & D$onset >= VAX_START, ], "Y_episode", "V_status", A1)),
  verow("A. Any recorded vaccination (Report 001 definition)", "Excluding cross-record links", "Firth penalised", ffit(Dwp, "Y_episode", "V_status")),
  verow("A. Any recorded vaccination (Report 001 definition)", "Excluding specimens taken after death", "Firth penalised", ffit(D[D$sdead == "No", ], "Y_episode", "V_status")),
  verow("A. Any recorded vaccination (Report 001 definition)", "Excluding malaria recorded", "Firth penalised", ffit(D[D$malaria == "No", ], "Y_episode", "V_status")),
  verow("B. Protocol definition (onset > 10 days after vaccination)", "Crude", "Firth penalised", ffit(D, "Y_episode", "V_protocol")),
  verow("B. Protocol definition (onset > 10 days after vaccination)", "Crude", "Exact conditional", xfit(D, "Y_episode", "V_protocol")),
  verow("B. Protocol definition (onset > 10 days after vaccination)", "Adjusted: age, sex, calendar time", "Firth penalised", ffit(D, "Y_episode", "V_protocol", A1)),
  verow("B. Protocol definition (onset > 10 days after vaccination)", "Adjusted: + contact and funeral", "Firth penalised", ffit(D, "Y_episode", "V_protocol", A2)),
  verow("B. Protocol definition (onset > 10 days after vaccination)", "Vaccinated after onset counted as unvaccinated", "Firth penalised", ffit(D, "Y_episode", "V_reclass")),
  verow("C. Vaccinated 0-10 days before onset (analysed separately)", "Crude", "Firth penalised", ffit(D, "Y_episode", "V_early")),
  verow("D. Alternative outcome definitions (any recorded vaccination)", "First RT-PCR result", "Firth penalised", ffit(D, "Y_first", "V_status")),
  verow("D. Alternative outcome definitions (any recorded vaccination)", "Final RT-PCR result", "Firth penalised", ffit(D, "Y_last", "V_status")),
  verow("D. Alternative outcome definitions (any recorded vaccination)", "Any positive RT-PCR result", "Firth penalised", ffit(D, "Y_any", "V_status")))

# ---- 8. Subgroup analyses ------------------------------------------------------
sg_defs <- list(
  list("Age group", "agegrp3", c("0-17","18-44","45+")),
  list("Sex", "sex_lbl", c("Female","Male")),
  list("Period of symptom onset", "period", c("Onset before 13 Sep 2025","Onset from 13 Sep 2025")),
  list("Contact with a case", "contact_c", c("Yes","No")),
  list("Took part in a funeral", "funeral", c("Yes","No")),
  list("Hospitalised", "hosp", c("Yes","No")),
  list("Latent exposure class", "lclass", cls_names),
  list("Linkage tier", "tier2", c("Tier 1 (exact)","Tier 2 (token)","Tier 3-4 / cross-record")))
subgroups <- do.call(rbind, lapply(sg_defs, function(s) {
  v <- s[[2]]; dd <- PS[as.character(PS[[v]]) %in% s[[3]], ]; dd$sg <- factor(as.character(dd[[v]]), levels = s[[3]])
  pint <- tryCatch({ m1 <- logistf(Y_episode ~ V_status * sg, data = dd, pl = FALSE); m0 <- logistf(Y_episode ~ V_status + sg, data = dd, pl = FALSE)
    anova(m1, m0)$pval }, error = function(e) NA)
  do.call(rbind, lapply(seq_along(s[[3]]), function(i) { r <- ffit(dd[dd$sg == s[[3]][i], ], "Y_episode", "V_status")
    x <- verow(s[[1]], s[[3]][i], "Firth penalised", r); x$`Interaction p` <- if (i == 1) pf(pint) else ""; x })) }))

# ---- 9. Death and severe disease ---------------------------------------------
CS <- D[D$Y_episode %in% 1, ]
# 9a. VE against death among confirmed cases (cohort): risk ratio + Firth OR
cohort_row <- function(label, expvar) { dd <- CS[!is.na(CS[[expvar]]) & !is.na(CS$died_bin), ]
  a <- sum(dd[[expvar]] == 1 & dd$died_bin == 1); nV <- sum(dd[[expvar]] == 1); b <- sum(dd[[expvar]] == 0 & dd$died_bin == 1); nU <- sum(dd[[expvar]] == 0)
  r <- ffit(dd, "died_bin", expvar); ve <- (1 - c(r$OR, r$U, r$L)) * 100
  data.frame(Exposure = label, `Deaths / vaccinated cases` = sprintf("%d / %d (%.1f%%)", a, nV, ifelse(nV > 0, 100 * a / nV, 0)),
    `Deaths / unvaccinated cases` = sprintf("%d / %d (%.1f%%)", b, nU, ifelse(nU > 0, 100 * b / nU, 0)),
    `Risk ratio` = ifelse(nV > 0 && nU > 0 && b > 0, f2((a / nV) / (b / nU)), "NA"),
    `OR (95% CI), Firth` = ifelse(is.na(r$OR), "not estimable", sprintf("%s (%s to %s)", f2(r$OR), f2(r$L), f2(r$U))),
    `VE against death % (95% CI)` = ifelse(is.na(r$OR), "not estimable", sprintf("%s (%s to %s)", f1(ve[1]), f1(ve[2]), f1(ve[3]))),
    check.names = FALSE) }
death_cohort <- rbind(cohort_row("Any recorded vaccination", "V_status"), cohort_row("Protocol definition (> 10 days)", "V_protocol"))
# 9b. Test-negative VE against fatal and against hospitalised Ebola disease
D$Y_fatal <- ifelse(D$Y_episode == 0, 0L, ifelse(D$Y_episode == 1 & D$died == "Yes", 1L, NA_integer_))
D$Y_hosp  <- ifelse(D$Y_episode == 0, 0L, ifelse(D$Y_episode == 1 & D$hosp == "Yes", 1L, NA_integer_))
severe_ve <- rbind(
  verow("Severe outcomes (test-negative)", "Fatal Ebola disease vs test-negative controls", "Firth penalised", ffit(D, "Y_fatal", "V_status")),
  verow("Severe outcomes (test-negative)", "Fatal Ebola disease, adjusted age and sex", "Firth penalised", ffit(D, "Y_fatal", "V_status", c("agegrp","sex_f"))),
  verow("Severe outcomes (test-negative)", "Hospitalised Ebola disease vs test-negative controls", "Firth penalised", ffit(D, "Y_hosp", "V_status")),
  verow("Severe outcomes (test-negative)", "Fatal Ebola disease, protocol definition", "Firth penalised", ffit(D, "Y_fatal", "V_protocol")))
# 9c. timing of death and viral load among cases
CS$onset_to_death <- as.numeric(asDate(CS$death_date) - CS$onset); CS$onset_to_death[CS$onset_to_death < 0 | CS$onset_to_death > 120] <- NA
mt <- function(x) if (sum(!is.na(x))) sprintf("%.0f (%.0f-%.0f), n=%d", median(x, na.rm = TRUE), quantile(x, .25, na.rm = TRUE), quantile(x, .75, na.rm = TRUE), sum(!is.na(x))) else "-"
death_detail <- data.frame(Measure = c("Confirmed cases, n", "Died, n (%)", "Specimen taken after death, n (%)",
    "Days from onset to death, median (IQR)", "Lowest GeneXpert Ct (GP), median (IQR): all cases",
    "   vaccinated cases", "   unvaccinated cases", "   cases who died", "   cases who survived"),
  Value = c(nrow(CS), sprintf("%d (%.1f%%)", sum(CS$died == "Yes"), 100 * mean(CS$died == "Yes")),
    sprintf("%d (%.1f%%)", sum(CS$sdead == "Yes"), 100 * mean(CS$sdead == "Yes")), mt(CS$onset_to_death),
    mt(CS$ct_gp), mt(CS$ct_gp[CS$V_status %in% 1]), mt(CS$ct_gp[CS$V_status %in% 0]), mt(CS$ct_gp[CS$died == "Yes"]), mt(CS$ct_gp[CS$died == "No"])),
  check.names = FALSE)
ct_p_vacc <- pf(tryCatch(wilcox.test(ct_gp ~ V_status, data = CS[!is.na(CS$V_status), ], exact = FALSE)$p.value, error = function(e) NA))
ct_p_died <- pf(tryCatch(wilcox.test(ct_gp ~ died, data = CS[CS$died %in% c("Yes","No"), ], exact = FALSE)$p.value, error = function(e) NA))
# death predictors among cases (age, sex, vaccination, hospitalisation): Firth
death_model <- tryCatch({ dm <- CS[!is.na(CS$died_bin) & !is.na(CS$agegrp3) & CS$sex %in% c("Male","Female"), ]
  m <- logistf(died_bin ~ agegrp3 + sex_f + I(V_status %in% 1), data = dm, pl = TRUE)
  data.frame(Term = c("Age 18-44 vs 0-17","Age 45+ vs 0-17","Male vs female","Vaccinated (any) vs not/unknown"),
    `OR (95% CI)` = sprintf("%s (%s to %s)", f2(exp(coef(m)[-1])), f2(exp(m$ci.lower[-1])), f2(exp(m$ci.upper[-1]))),
    p = pf(m$prob[-1]), N = nrow(dm), check.names = FALSE, row.names = NULL) }, error = function(e) data.frame())

# ---- 10. Co-infection (malaria) ----------------------------------------------
coinf <- rbind(cat_rows(cc, "grp", "malaria", "Malaria recorded by the laboratory", c("Yes","No"), drop_unknown = character(0)),
               cat_rows(cc, "grp", "biofire_tested", "Tested on the BioFire panel", c("Yes","No"), drop_unknown = character(0)))
coinf_cases_died <- sprintf("%d of %d confirmed cases with malaria recorded died", sum(CS$malaria == "Yes" & CS$died == "Yes"), sum(CS$malaria == "Yes"))

# ---- 11. Epidemic curve data (weekly, by onset) --------------------------------
epi <- cc[!is.na(cc$onset), ]; epi$week <- as.Date(cut(epi$onset, "week", start.on.monday = TRUE))
epi$cat <- ifelse(epi$Y_episode == 1, ifelse(epi$V_status %in% 1, "Case, vaccinated", "Case, not vaccinated/unknown"), "Test-negative control")
epicurve <- as.data.frame(table(week = epi$week, group = epi$cat)); epicurve <- epicurve[epicurve$Freq > 0, ]
epicurve$week <- as.Date(as.character(epicurve$week))
vacc_week <- { v <- D[!is.na(D$vacc_dt) & D$vacc_status == "Vaccinated", ]; if (nrow(v)) as.data.frame(table(week = as.Date(cut(v$vacc_dt, "week", start.on.monday = TRUE)))) else data.frame() }

# ---- 12. Figures ---------------------------------------------------------------
FIGD <- file.path(OUT_DIR, "figures")
NAVY <- "#1F4E79"; GREEN <- "#4E7D3A"; AMB <- "#C77C1E"; GREY <- "#9E9E9E"; RED <- "#A23B2C"
thf <- theme_minimal(base_size = 11, base_family = "sans") + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", size = 12),
  plot.title.position = "plot", legend.position = "bottom")
sv <- function(f, p, w = 7.5, h = 4.5) suppressWarnings(ggsave(file.path(FIGD, f), p, width = w, height = h, dpi = 200, bg = "white"))

# F6 epidemic curve
try(sv("F6_epicurve.png", ggplot(epicurve[epicurve$week >= as.Date("2025-07-28"), ], aes(week, Freq, fill = group)) + geom_col(width = 6) +
  geom_vline(xintercept = VAX_START, linetype = "dashed", colour = "grey30") +
  annotate("text", x = VAX_START, y = Inf, label = "Vaccination starts (13 Sep 2025)", hjust = -0.05, vjust = 1.5, size = 3) +
  scale_fill_manual(values = c("Case, vaccinated" = AMB, "Case, not vaccinated/unknown" = RED, "Test-negative control" = "#B7C9DA"), name = NULL) +
  scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks") + labs(title = "Participants by week of symptom onset", x = "Week of onset", y = "Participants") + thf, 8.5, 4.5), silent = TRUE)

# F7 main VE forest (classic) – sections A and B
fpl <- ve_table[!is.na(ve_table$VE) & is.finite(ve_table$VE_L), ]
fpl$lab <- paste0(substr(fpl$Section, 1, 1), ". ", fpl$Analysis, " (", fpl$Method, ")"); fpl$lab <- factor(fpl$lab, levels = rev(unique(fpl$lab)))
fpl$sec <- substr(fpl$Section, 1, 1)
try(sv("F7_VE_all_methods.png", ggplot(fpl, aes(VE, lab, colour = sec)) + geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_errorbar(aes(xmin = pmax(VE_L, -100), xmax = pmin(VE_U, 100)), width = 0.25, orientation = "y") + geom_point(size = 2.4) +
  scale_colour_manual(values = c(A = NAVY, B = GREEN, C = AMB, D = GREY), labels = c(A = "Any recorded vaccination", B = "Protocol definition (> 10 days)",
    C = "Vaccinated 0-10 days", D = "Alternative outcome definitions"), name = NULL) +
  coord_cartesian(xlim = c(-100, 100)) + labs(title = "Vaccine effectiveness by analysis and method", subtitle = "Points: VE; bars: 95% CI (truncated at -100%)",
  x = "Vaccine effectiveness (%)", y = NULL) + thf + guides(colour = guide_legend(nrow = 2)), 9, 7), silent = TRUE)

# F8 subgroup forest
sgp <- subgroups[!is.na(subgroups$VE) & is.finite(subgroups$VE_L), ]
if (nrow(sgp)) { sgp$lab <- paste0(sgp$Section, ": ", sgp$Analysis, "  [", sgp$`Vaccinated / cases`, "]"); sgp$lab <- factor(sgp$lab, levels = rev(sgp$lab))
  try(sv("F8_subgroups.png", ggplot(sgp, aes(VE, lab)) + geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_errorbar(aes(xmin = pmax(VE_L, -150), xmax = pmin(VE_U, 100)), width = 0.25, orientation = "y", colour = NAVY) + geom_point(size = 2.4, colour = NAVY) +
    coord_cartesian(xlim = c(-150, 100)) + labs(title = "Vaccine effectiveness by subgroup (Firth, crude)", subtitle = "Bracket: vaccinated / all cases in the subgroup; lower limits truncated at -150%",
    x = "Vaccine effectiveness (%)", y = NULL) + thf, 9, 6.5), silent = TRUE) }

# F9 latent class profiles
lp <- data.frame(Indicator = rep(unname(LCI), Kbest), Class = factor(rep(cls_names, each = length(LCI)), levels = cls_names), p = as.vector(t(LC$theta)) * 100)
try(sv("F9_latent_classes.png", ggplot(lp, aes(Indicator, p, fill = Class)) + geom_col(position = position_dodge(0.8), width = 0.75) + coord_flip() +
  scale_fill_manual(values = c(GREY, NAVY, GREEN, AMB)[seq_len(Kbest)], name = NULL) +
  labs(title = sprintf("Latent exposure-risk classes (%d-class model)", Kbest), x = NULL, y = "Probability of reporting the exposure (%)") + thf, 8, 4.8), silent = TRUE)

# F10 covariate balance (IPTW)
bl <- pivot_longer(balance, -Covariate, names_to = "When", values_to = "SMD")
try(sv("F10_ps_balance.png", ggplot(bl, aes(abs(SMD), Covariate, shape = When, colour = When)) + geom_vline(xintercept = 0.1, linetype = "dashed", colour = "grey40") +
  geom_point(size = 2.6) + scale_colour_manual(values = c(GREY, NAVY), name = NULL) + scale_shape_manual(values = c(1, 16), name = NULL) +
  labs(title = "Covariate balance before and after weighting", subtitle = "Absolute standardised mean difference; dashed line = 0.1", x = "|SMD|", y = NULL) + thf, 8, 4.5), silent = TRUE)

# F11 Ct values among cases
ctd <- CS[!is.na(CS$ct_gp) & !is.na(CS$V_status), ]; ctd$vg <- ifelse(ctd$V_status == 1, "Vaccinated", "Not vaccinated")
if (nrow(ctd) >= 5) try(sv("F11_ct_values.png", ggplot(ctd, aes(vg, ct_gp)) + geom_boxplot(width = 0.5, outlier.shape = NA, fill = "#DCE6F0") +
  geom_jitter(width = 0.12, height = 0, size = 1.6, colour = NAVY) + labs(title = "Lowest GeneXpert Ct value (GP) among confirmed cases",
  subtitle = paste("Lower Ct = higher viral load; Wilcoxon p =", ct_p_vacc), x = NULL, y = "Ct value") + thf, 6, 4.5), silent = TRUE)

# F12 age distribution of cases and controls
try(sv("F12_age_by_status.png", ggplot(cc[!is.na(cc$age), ], aes(age, fill = grp)) + geom_histogram(binwidth = 5, boundary = 0, colour = "white") +
  facet_wrap(~grp, ncol = 1, scales = "free_y") + scale_fill_manual(values = c(Cases = RED, Controls = NAVY), guide = "none") +
  labs(title = "Age of cases and test-negative controls", x = "Age (years)", y = "Participants") + thf, 7, 5), silent = TRUE)

# F13 vaccination-to-onset interval among vaccinated (cases vs controls)
vt <- D[!is.na(D$vax_to_onset) & !is.na(D$Y_episode), ]; vt$grp <- ifelse(vt$Y_episode == 1, "Cases", "Controls")
if (nrow(vt)) try(sv("F13_vax_to_onset.png", ggplot(vt, aes(vax_to_onset, fill = grp)) + geom_histogram(binwidth = 5, boundary = 0, colour = "white") +
  geom_vline(xintercept = PROTOCOL_WINDOW_DAYS, linetype = "dashed") + scale_fill_manual(values = c(Cases = RED, Controls = NAVY), name = NULL) + coord_cartesian(xlim = c(-30, 60)) +
  labs(title = "Days from vaccination to symptom onset", subtitle = "Dashed line: 10-day protocol threshold; axis limited to -30 to 60 days", x = "Days", y = "Participants") + thf, 7.5, 4.3), silent = TRUE)

# F14 calendar time: cases per fortnight vs share of controls vaccinated (primary sample)
ft_d <- PS[!is.na(PS$fortnight), ]
fortnight_tab <- do.call(rbind, lapply(sort(unique(ft_d$fortnight)), function(f) { d <- ft_d[ft_d$fortnight == f, ]
  data.frame(Fortnight_start = f, Cases = sum(d$Y_episode == 1), `Vaccinated cases` = sum(d$Y_episode == 1 & d$V_status == 1),
    Controls = sum(d$Y_episode == 0), `Vaccinated controls` = sum(d$Y_episode == 0 & d$V_status == 1),
    `% controls vaccinated` = ifelse(sum(d$Y_episode == 0) > 0, round(100 * mean(d$V_status[d$Y_episode == 0] == 1), 1), NA), check.names = FALSE) }))
fp14 <- fortnight_tab[as.Date(fortnight_tab$Fortnight_start) >= as.Date("2025-07-01"), ]; fp14$date <- as.Date(fp14$Fortnight_start) + 7
sc <- max(fp14$Cases, 1) / 100
try(sv("F14_calendar_confounding.png", ggplot(fp14, aes(date)) + geom_col(aes(y = Cases), fill = RED, width = 11) +
  geom_line(aes(y = `% controls vaccinated` * sc), colour = NAVY, linewidth = 1) + geom_point(aes(y = `% controls vaccinated` * sc), colour = NAVY, size = 2) +
  geom_vline(xintercept = VAX_START, linetype = "dashed", colour = "grey30") +
  scale_y_continuous(name = "Confirmed cases (bars)", sec.axis = sec_axis(~ . / sc, name = "% of test-negative controls vaccinated (line)")) +
  scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks") +
  labs(title = "Cases occurred before vaccination coverage rose", subtitle = "Primary analytic sample by fortnight of symptom onset; dashed line = start of vaccination (13 Sep 2025)", x = "Fortnight of onset") + thf, 8.5, 4.6), silent = TRUE)

# timing intervals (days) and date plausibility
iv <- function(x) { x <- x[!is.na(x)]; if (length(x)) c(sprintf("%d", length(x)), sprintf("%.0f", median(x)), sprintf("%.0f to %.0f", quantile(x, .25), quantile(x, .75)),
  sprintf("%.0f to %.0f", min(x), max(x))) else c("0", "-", "-", "-") }
intervals <- data.frame(t(sapply(list(
  `Symptom onset to episode-defining specimen` = D$onset_to_swab,
  `Vaccination to symptom onset (vaccinated)` = D$vax_to_onset,
  `Symptom onset to death (confirmed cases)` = CS$onset_to_death), iv)), check.names = FALSE)
names(intervals) <- c("n", "Median", "IQR", "Range"); intervals <- cbind(Interval = rownames(intervals), intervals); rownames(intervals) <- NULL
date_checks <- data.frame(Check = c("Specimen more than 30 days after onset", "Specimen before onset", "Onset before 1 July 2025", "Onset after 30 November 2025"),
  N = c(sum(D$onset_to_swab > 30, na.rm = TRUE), sum(D$onset_to_swab < 0, na.rm = TRUE),
        sum(D$onset < as.Date("2025-07-01"), na.rm = TRUE), sum(D$onset > as.Date("2025-11-30"), na.rm = TRUE)), check.names = FALSE)

# ---- 13. Save ----------------------------------------------------------------
ext <- list(table1 = table1, table2_by_vaccination = table2, pregnancy = preg_tab, vacc_details = vacc_details,
  ve_table = ve_table, subgroups = subgroups, iptw_balance = balance, lca_fit = lca_fit_tbl, lca_profile = lca_profile,
  lca_by_status = lca_by_status, death_cohort = death_cohort, severe_ve = severe_ve, death_detail = death_detail,
  death_model = death_model, coinfection = coinf, epicurve = epicurve, vacc_week = vacc_week,
  intervals = intervals, date_checks = date_checks, fortnight = fortnight_tab,
  notes = data.frame(item = c("Latent classes chosen", "IPTW sample", "PS covariates", "Ct p (vaccinated vs not)", "Ct p (died vs survived)",
                              "Malaria among cases", "LCA participants classified"),
                     value = c(Kbest, nrow(ipd), paste(PSV, collapse = ", "), ct_p_vacc, ct_p_died, coinf_cases_died, sum(keepY))))
saveRDS(ext, file.path(OUT_DIR, "extended_bundle.rds"))
strip <- function(x) x[, !names(x) %in% c("VE","VE_L","VE_U"), drop = FALSE]
writexl::write_xlsx(list(T1_cases_controls = table1, T2_by_vaccination = table2, T_pregnancy = preg_tab, T_vacc_details = vacc_details,
  T_VE_all_methods = strip(ve_table), T_subgroups = strip(subgroups), T_IPTW_balance = balance, T_LCA_fit = lca_fit_tbl,
  T_LCA_profile = lca_profile, T_LCA_by_status = lca_by_status, T_death_cohort = death_cohort, T_severe_VE = strip(severe_ve),
  T_death_detail = death_detail, T_death_model = death_model, T_coinfection = coinf, T_epicurve = epicurve, T_vacc_week = vacc_week,
  T_intervals = intervals, T_date_checks = date_checks, T_fortnight = fortnight_tab,
  Notes = ext$notes), file.path(OUT_DIR, "extended_tables.xlsx"))

cat("[04] extended analysis (aggregate results only)\n")
for (i in seq_len(nrow(ve_table))) cat(sprintf("     %-3s %-55s %-28s VE %s\n", substr(ve_table$Section[i], 1, 2), ve_table$Analysis[i], ve_table$Method[i], ve_table$`VE % (95% CI)`[i]))
cat(sprintf("[04] latent classes: %d chosen (BIC); IPTW n=%d; subgroups=%d rows\n", Kbest, nrow(ipd), nrow(subgroups)))
cat(sprintf("[04] tables -> %s\n", file.path(OUT_DIR, "extended_tables.xlsx")))
