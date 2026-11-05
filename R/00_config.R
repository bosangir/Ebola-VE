# =============================================================================
#  00_config.R  —  packages, paths, shared helpers, coding maps, VE transform.
#  Sourced first by run_all.R and by every stage. No participant data is read here.
#  Private input/output paths live in config.local.R (gitignored); see
#  config.local.R.example for the template.
# =============================================================================

## ---- packages (report stage additionally needs officer, flextable, readxl) ----
.need <- c("dplyr", "ggplot2", "writexl")
.missing <- .need[!vapply(.need, requireNamespace, logical(1), quietly = TRUE)]
if (length(.missing))
  stop("Install missing package(s): install.packages(c(",
       paste(sprintf('"%s"', .missing), collapse = ", "), "))")
suppressWarnings(suppressMessages({library(dplyr); library(ggplot2); library(writexl)}))

## ---- project root + local paths ----------------------------------------------
if (!exists("PROJ")) PROJ <- getwd()                      # run_all.R sets this; else cwd
# Ignore any stale LINK_INPUT/WIDE_LAB/DASH_DATA left in an interactive session;
# config.local.R (if present) re-sets them, otherwise they resolve from data/ below.
LINK_INPUT <- WIDE_LAB <- DASH_DATA <- NULL
local_cfg <- file.path(PROJ, "config.local.R")
if (file.exists(local_cfg)) {
  source(local_cfg, local = FALSE)
} else {
  message("[config] config.local.R not found — using the local data/ folder. ",
          "Copy config.local.R.example to config.local.R only if you need to point elsewhere.")
}
# Default input location: a local, gitignored  data/  folder — drop the linked files
# there and the pipeline finds them (no config edit needed). config.local.R overrides.
# Only file EXISTENCE is checked here; no data content is read.
.pick <- function(cands){ hit <- cands[file.exists(cands)]; if (length(hit)) hit[1] else NA_character_ }
if (is.null(LINK_INPUT)) LINK_INPUT <- .pick(file.path(PROJ, "data",
    c("cleaned_linkage.csv","linked_dataset_MAIN_distinct_cases.csv","linked_confirmed.csv",
      "linked_dataset_MAIN.csv","linked_database.csv","linkage.csv","linked.csv")))
if (is.null(WIDE_LAB))   WIDE_LAB   <- .pick(file.path(PROJ, "data", c("merged_eve_lab_wide.csv","wide_lab.csv")))
if (is.null(DASH_DATA))  DASH_DATA  <- {                    # dashboard JSON folder (optional)
    d <- file.path(PROJ, "data"); if (file.exists(file.path(d, "cleaning_status.json"))) d else "" }
# Require the linkage input. The wide lab register is OPTIONAL: if absent, the RT-PCR
# result is taken from the linkage file itself (single result; no multi-specimen episode history).
if (is.na(LINK_INPUT))
  stop("No linkage input found.\n  -> put the linked file in data/ (see data/README.md), ",
       "or set LINK_INPUT in config.local.R.")
if (is.na(WIDE_LAB)) { WIDE_LAB <- ""
  message("[config] No wide lab register (merged_eve_lab_wide.csv) in data/ — the RT-PCR result will ",
          "come from the linkage file (single result per participant). Add it to data/ for full episode history.") }
# Guard: don't mix a data/ file with a config.local.R path pointing elsewhere.
.indir <- function(p, sub) startsWith(gsub("\\\\","/", normalizePath(p, mustWork = FALSE)),
                                      gsub("\\\\","/", normalizePath(file.path(PROJ, sub), mustWork = FALSE)))
if (nzchar(WIDE_LAB) && (.indir(LINK_INPUT,"data") != .indir(WIDE_LAB,"data")))
  stop("Input source mismatch — the linkage and wide-lab files are in different places:\n  linkage  = ", LINK_INPUT,
       "\n  wide lab = ", WIDE_LAB,
       "\n-> put BOTH in data/ (see data/README.md), or set both LINK_INPUT and WIDE_LAB in config.local.R.")
if (!exists("OUT_DIR"))    OUT_DIR    <- file.path(PROJ, "outputs")
if (!exists("CODED_OUT"))  CODED_OUT  <- file.path(OUT_DIR, "Distinct_CODED.csv")  # cohort-stage output
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT_DIR, "figures"), showWarnings = FALSE, recursive = TRUE)

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
pdate  <- function(x){ x <- trimws(blank(x)); out <- as.Date(rep(NA_real_, length(x)), origin = "1970-01-01")
  for (f in c("%Y-%m-%d","%d-%b-%Y","%d-%b-%y","%d/%m/%Y","%d/%m/%y","%d-%m-%Y","%d.%m.%Y")) {
    mm <- is.na(out) & nzchar(x); if (!any(mm)) break; out[mm] <- suppressWarnings(as.Date(x[mm], format = f)) }
  ser <- is.na(out) & grepl("^[0-9]{5}$", x); if (any(ser)) out[ser] <- as.Date(as.numeric(x[ser]), origin = "1899-12-30")
  as.character(out) }
# Vaccine effectiveness from an odds ratio and its CI:  VE = (1 - OR) * 100
ve_from_or <- function(or, lo, hi){ or <- unname(or); lo <- unname(lo); hi <- unname(hi)
  c(VE = (1 - or) * 100, VE_L = (1 - hi) * 100, VE_U = (1 - lo) * 100) }

cat(sprintf("[config] project = %s\n[config] linkage input = %s\n[config] outputs -> %s\n",
            PROJ, LINK_INPUT, OUT_DIR))

