# =============================================================================
#  00_config.R  -  packages, folders, shared helpers, coding maps, VE transform.
#  Sourced first by run_all.R. No participant data is read here.
#
#  Two ways to run:
#   * FULL (private) mode  - on the study team's computer, where the raw
#     identifiable inputs exist in the private folder. Rebuilds data/ from the
#     414-record linkage export, applies the site QC feedback, removes
#     duplicates and de-identifies, then runs the analysis.
#   * ANALYSIS-ONLY (public) mode - anywhere else. Uses the de-identified
#     dataset already in data/ and runs the analysis only.
#  Set the environment variable EVE_MODE=public to force analysis-only mode.
# =============================================================================

## ---- packages ------------------------------------------------------------------
.need <- c("dplyr", "ggplot2", "writexl", "logistf", "sandwich", "tidyr", "survival")
.missing <- .need[!vapply(.need, requireNamespace, logical(1), quietly = TRUE)]
if (length(.missing))
  stop("Install missing package(s): install.packages(c(",
       paste(sprintf('"%s"', .missing), collapse = ", "), "))")
suppressWarnings(suppressMessages({library(dplyr); library(ggplot2); library(writexl)}))

## ---- repository folders (shareable) ---------------------------------------------
if (!exists("PROJ")) PROJ <- getwd()                    # run_all.R sets this to the repository root
SCRIPTS  <- file.path(PROJ, "R")
DATA_DIR <- file.path(PROJ, "data")                     # de-identified dataset only
OUT_DIR  <- file.path(PROJ, "Outputs")                  # aggregate tables and figures only
DEID_OUT <- file.path(DATA_DIR, "EVE_VE_analysis_dataset_deidentified.csv")
DICT_OUT <- file.path(PROJ, "data_dictionary", "DATA_DICTIONARY.csv")

## ---- private folder (never uploaded) ------------------------------------------
# Raw identifiable inputs and every file that names a person live OUTSIDE the
# repository, in a folder next to it. Override with EVE_PRIVATE_DIR or config.local.R.
PRIVATE_DIR <- Sys.getenv("EVE_PRIVATE_DIR", file.path(dirname(PROJ), "EVE_VE_PRIVATE_identifiers"))
RAW_DIR     <- file.path(PRIVATE_DIR, "raw_inputs")
LINK_INPUT  <- file.path(RAW_DIR, "EVE_raw_linkage_export_414_records.csv")   # confirmed linkage export
WIDE_LAB    <- file.path(RAW_DIR, "EVE_raw_lab_register_wide.csv")            # triangulated lab register
DASH_DATA   <- file.path(RAW_DIR, "site_qc_feedback")                         # site QC decisions (JSON)
local_cfg <- file.path(PROJ, "config.local.R")                                # optional, gitignored
if (file.exists(local_cfg)) source(local_cfg, local = FALSE)
CODED_OUT <- file.path(PRIVATE_DIR, "EVE_VE_dataset_identifiable.csv")

## ---- mode ----------------------------------------------------------------------
PRIVATE_MODE <- file.exists(LINK_INPUT) && !identical(tolower(Sys.getenv("EVE_MODE")), "public")
if (PRIVATE_MODE) {
  if (!file.exists(WIDE_LAB)) { WIDE_LAB <- ""
    message("[config] No wide lab register: the RT-PCR result comes from the linkage file (single result).") }
  if (!file.exists(file.path(DASH_DATA, "record_corrections.json")) &&
      !file.exists(file.path(DASH_DATA, "merge_decisions.json"))) { DASH_DATA <- ""
    message("[config] WARNING: no site QC feedback found; corrections and merges will NOT be applied.") }
  dir.create(PRIVATE_DIR, showWarnings = FALSE, recursive = TRUE)
} else {
  if (!file.exists(DEID_OUT))
    stop("Analysis-only mode needs the de-identified dataset: ", DEID_OUT)
  message("[config] Analysis-only mode: using the de-identified dataset in data/.")
}
for (.d in c(DATA_DIR, dirname(DICT_OUT), OUT_DIR, file.path(OUT_DIR, "figures")))
  dir.create(.d, showWarnings = FALSE, recursive = TRUE)

## ---- constants (protocol / coding schemes, from the data dictionary) ---------
PROTOCOL_WINDOW_DAYS <- 10          # Ervebo: symptom onset > 10 days after vaccination = effective
AGE_BREAKS <- c(-Inf, 5, 17, 44, Inf)
AGE_LABELS <- c("0-5", "6-17", "18-44", "45+")
YN  <- c("1"="Yes","2"="No","99"="Unknown","0"="Unknown","3"="Other")
SEX <- c("1"="Male","2"="Female","3"="Unknown")
VAC <- c("1"="Vaccinated","2"="Not vaccinated","99"="Unknown")
VIT <- c("1"="Alive","2"="Dead","99"="Unknown")
TYP <- c("1"="rVSV-ZEBOV (Ervebo)","99"="Unknown type","2"="Ad26/MVA (Zabdeno/Mvabea)")

## ---- helpers -----------------------------------------------------------------
rc     <- function(p) read.csv(p, colClasses = "character", check.names = FALSE,
                               na.strings = character(0), encoding = "UTF-8")
blank  <- function(x){ if (is.null(x)) return(character(0)); x[is.na(x)] <- ""; trimws(x) }
numv   <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.\\-]", "", blank(x))))
asDate <- function(x) suppressWarnings(as.Date(ifelse(blank(x) == "", NA, blank(x))))
g      <- function(df, c) if (c %in% names(df)) blank(df[[c]]) else rep("", nrow(df))
norm   <- function(s){ s <- blank(s); t <- suppressWarnings(iconv(s, to = "ASCII//TRANSLIT"))
                       s <- ifelse(is.na(t), s, t); toupper(gsub("\\s+", " ", trimws(s))) }
isRealId <- function(id){ id <- toupper(blank(id)); nzchar(id) & grepl("[0-9]", id) & !grepl("^(RAS|INCONNU|RDC|NA)$", id) }
# canonical participant key: real case-id if present, else standardized full name
pkey   <- function(n1, n4, n5) ifelse(isRealId(n1), paste0("ID#", norm(n1)),
                                      paste0("NM#", norm(paste0(n4, " ", n5))))
# Yes/No/Unknown recode robust to coded (1/2/99) or text (Yes/Oui/No/Non)
yn     <- function(x){ x <- toupper(trimws(blank(x)))
  ifelse(x %in% c("1","YES","OUI","Y","O","TRUE"), "Yes",
  ifelse(x %in% c("2","NO","NON","N","FALSE","0"), "No", "Unknown")) }
# tolerant multi-format date parser -> "YYYY-MM-DD" character
.MONTHS <- c(january=1,february=2,march=3,april=4,may=5,june=6,july=7,august=8,september=9,october=10,
             november=11,december=12,janvier=1,fevrier=2,mars=3,avril=4,mai=5,juin=6,juillet=7,aout=8,
             septembre=9,octobre=10,novembre=11,decembre=12)
pdate  <- function(x){ x <- trimws(blank(x)); out <- as.Date(rep(NA_real_, length(x)), origin = "1970-01-01")
  # "13 October 2025" / "13 octobre 2025" — parsed explicitly so it works under any system locale
  mo <- regmatches(x, regexec("^([0-9]{1,2})[ -]+([A-Za-zéû]+)[ -]+([0-9]{4})$", x))
  for (i in which(lengths(mo) == 4)) { k <- .MONTHS[chartr("éû", "eu", tolower(mo[[i]][3]))]
    if (!is.na(k)) out[i] <- suppressWarnings(as.Date(sprintf("%s-%02d-%02d", mo[[i]][4], k, as.integer(mo[[i]][2])))) }
  for (f in c("%Y-%m-%d","%d-%b-%Y","%d-%b-%y","%d/%m/%Y","%d/%m/%y","%d-%m-%Y","%d.%m.%Y")) {
    mm <- is.na(out) & nzchar(x); if (!any(mm)) break; out[mm] <- suppressWarnings(as.Date(x[mm], format = f)) }
  ser <- is.na(out) & grepl("^[0-9]{5}$", x); if (any(ser)) out[ser] <- as.Date(as.numeric(x[ser]), origin = "1899-12-30")
  # Two-digit years: surveillance dates are "08-Sep-25". A %Y format reads "25" as year
  # 25 AD, so any parsed year below 100 is moved to 20xx. (Before this fix every
  # surveillance date was ~2000 years early relative to the ISO laboratory dates.)
  yr <- as.integer(format(out, "%Y")); fx <- !is.na(yr) & yr < 100
  if (any(fx)) out[fx] <- as.Date(sprintf("%04d-%s", yr[fx] + 2000L, format(out[fx], "%m-%d")))
  as.character(out) }
# Vaccine effectiveness from an odds ratio and its CI:  VE = (1 - OR) * 100
ve_from_or <- function(or, lo, hi){ or <- unname(or); lo <- unname(lo); hi <- unname(hi)
  c(VE = (1 - or) * 100, VE_L = (1 - hi) * 100, VE_U = (1 - lo) * 100) }

cat(sprintf("[config] repository = %s\n[config] mode = %s\n[config] de-identified data = %s\n[config] aggregate outputs -> %s\n",
            PROJ, if (PRIVATE_MODE) "FULL (rebuilds data/ from private raw inputs)" else "ANALYSIS-ONLY", DEID_OUT, OUT_DIR))
if (PRIVATE_MODE) cat(sprintf("[config] private folder (not in repository) = %s\n", PRIVATE_DIR))

