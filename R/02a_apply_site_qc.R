# =============================================================================
#  02a_apply_site_qc.R  —  apply the site QC feedback recorded in the dashboard to the
#  linkage rows BEFORE de-duplication. Sourced by 02_build_cohort.R.
#
#  Inputs (DASH_DATA folder, written by the dashboard / serve.R):
#    deletions.json           lab records removed
#    record_corrections.json  corrected field values, keyed WP|lab_id|surv_name|lab_name
#    merge_decisions.json     duplicate merges: master_rid + loser_rids
#    cleaning_status.json     query status (Corrected / Verified) — audit summary only
#
#  Matching of a QC key to linkage rows, in order:
#    1 exact key (lab ID + surveillance name + lab name)
#    2 lab ID + surveillance name
#    3 lab ID alone, only when that lab ID belongs to a single surveillance-lab pair
#  Anything else is NOT applied and is listed in the log for manual resolution.
#
#  Output: list(m = cleaned rows, log = one row per QC action, summary = counts)
#  The log identifies records by lab ID only (no names, no name values).
# =============================================================================
qc_rj <- function(p) if (file.exists(p)) tryCatch(jsonlite::fromJSON(paste(readLines(p, warn = FALSE, encoding = "UTF-8"),
                       collapse = ""), simplifyVector = FALSE), error = function(e) NULL) else NULL
qc_nid <- function(s) gsub("[^A-Z0-9]", "", toupper(blank(s)))

# field names used by the dashboard -> columns in the linkage file
QC_ALIAS <- list(symptom_onset_date = c("date_frist_sympt", "symptom_onset_date"),
                 specimen_collection_date = "specimen_collection_date",
                 age_years = "age_years", sex = "sex", name_4 = "name_4", name_5 = "name_5",
                 NAMES = c("labreg_name", "NAMES"), AGE = c("AGE", "age_lab"), SEX = c("SEX"))
QC_NAME_FIELDS <- c("name_4", "name_5", "NAMES")
QC_SURV_FIELDS <- c("symptom_onset_date", "specimen_collection_date", "age_years", "sex", "name_4", "name_5")

qc_value <- function(field, v) {                       # normalise a corrected value for its field
  v <- trimws(as.character(v)[1])
  if (field %in% c("symptom_onset_date", "specimen_collection_date")) return(pdate(v))
  if (field %in% c("sex")) { u <- toupper(v)
    return(ifelse(u %in% c("1","M","MALE","MASCULIN","H"), "1", ifelse(u %in% c("2","F","FEMALE","FEMININ","FÉMININ"), "2", v))) }
  if (field %in% c("age_years", "AGE")) { n <- numv(v); return(ifelse(is.na(n), "", as.character(n))) }
  v
}

apply_qc <- function(m, DASH_DATA) {
  log <- data.frame(type = character(0), lab_id = character(0), field = character(0), match = character(0),
                    status = character(0), detail = character(0), stringsAsFactors = FALSE)
  addlog <- function(type, lab_id, field, match, status, detail = "")
    log[nrow(log) + 1, ] <<- c(type, lab_id, field, match, status, detail)
  labname <- if ("labreg_name" %in% names(m)) g(m, "labreg_name") else g(m, "NAMES")
  uid <- g(m, "lab_sample_id"); sn <- qc_nid(paste0(g(m, "name_4"), g(m, "name_5"))); ln <- qc_nid(labname)
  # original participant key (before any correction): rows sharing it come from the same
  # surveillance record, so a correction to a surveillance field must reach all of them
  pk0 <- pkey(g(m, "name_1"), g(m, "name_4"), g(m, "name_5"))
  m$qc_corrected_fields <- ""; m$qc_merged <- ""

  resolve <- function(key) {                             # -> list(rows, method)
    p <- strsplit(key, "|", fixed = TRUE)[[1]]
    if (length(p) < 3 || p[1] != "WP") return(list(rows = integer(0), method = "unsupported key type"))
    r <- which(uid == p[2]); if (!length(r)) return(list(rows = integer(0), method = "lab ID not in linkage input"))
    if (length(p) >= 4) { e <- r[sn[r] == p[3] & ln[r] == p[4]]; if (length(e)) return(list(rows = e, method = "exact key")) }
    e <- r[sn[r] == p[3]]; if (length(e)) return(list(rows = e, method = "lab ID + surveillance name"))
    if (length(unique(sn[r])) == 1) return(list(rows = r, method = "lab ID (single pair)"))
    list(rows = integer(0), method = "ambiguous: lab ID linked to several people")
  }
  idof <- function(key) { p <- strsplit(key, "|", fixed = TRUE)[[1]]; if (length(p) >= 2) p[2] else "" }

  ## 1. deletions -----------------------------------------------------------
  del <- unlist(qc_rj(file.path(DASH_DATA, "deletions.json")))
  n_del <- 0
  if (length(del)) { drop <- uid %in% del; n_del <- sum(drop)
    for (d in del) addlog("deletion", d, "", "", ifelse(d %in% uid, "applied", "not in linkage input"))
    if (n_del) { m <- m[!drop, , drop = FALSE]; uid <- uid[!drop]; sn <- sn[!drop]; ln <- ln[!drop]; pk0 <- pk0[!drop] } }

  ## 2. record corrections ---------------------------------------------------
  corr <- qc_rj(file.path(DASH_DATA, "record_corrections.json")); if (is.null(corr)) corr <- list()
  known <- names(QC_ALIAS)
  for (key in names(corr)) {
    ce <- corr[[key]]; rr <- resolve(key)
    for (k in names(ce)) {
      v <- ce[[k]]; vv <- if (length(v)) as.character(v)[1] else ""; fld <- k; note <- ""
      if (!(fld %in% known) && vv %in% known) { fld <- vv; vv <- k; note <- "field/value were swapped; repaired" }
      if (!(fld %in% known)) { addlog("correction", idof(key), "(unrecognised)", rr$method, "not applied",
                                      "field name not recognised"); next }
      if (!length(rr$rows)) { addlog("correction", idof(key), fld, rr$method, "not applied", note); next }
      if (!nzchar(vv)) { addlog("correction", idof(key), fld, rr$method, "not applied", "empty value"); next }
      nv <- qc_value(fld, vv)
      if (!nzchar(nv)) { addlog("correction", idof(key), fld, rr$method, "not applied", "value could not be parsed"); next }
      cols <- QC_ALIAS[[fld]][QC_ALIAS[[fld]] %in% names(m)]
      if (!length(cols)) { addlog("correction", idof(key), fld, rr$method, "not applied",
                                  "lab-register field, not in linkage input"); next }
      rows <- if (fld %in% QC_SURV_FIELDS) which(pk0 %in% pk0[rr$rows]) else rr$rows   # all rows of that surveillance record
      for (cc in cols) m[rows, cc] <- nv
      m$qc_corrected_fields[rows] <- ifelse(nzchar(m$qc_corrected_fields[rows]),
                                            paste(m$qc_corrected_fields[rows], fld, sep = ";"), fld)
      addlog("correction", idof(key), fld, rr$method, "applied",
             trimws(paste(note, if (length(rows) > length(rr$rows)) sprintf("(also applied to %d linked row(s) of the same person)", length(rows) - length(rr$rows)) else "",
                          if (fld %in% QC_NAME_FIELDS) "(name value not logged)" else "")))
    }
  }

  ## 3. duplicate merges -------------------------------------------------------
  mg <- qc_rj(file.path(DASH_DATA, "merge_decisions.json")); if (is.null(mg)) mg <- list()
  mg <- mg[startsWith(names(mg), "MG|")]
  drop_rows <- integer(0)
  for (x in mg) {
    mr <- resolve(x$master_rid)
    if (!length(mr$rows)) { addlog("merge", idof(x$master_rid), "", mr$method, "not applied", "master not found"); next }
    master <- mr$rows[1]
    for (l in unlist(x$loser_rids)) {
      lr <- resolve(l)
      if (!length(lr$rows)) { addlog("merge", idof(l), "", lr$method, "not applied",
                                     "duplicate record not found in linkage input"); next }
      lr$rows <- setdiff(lr$rows, mr$rows)
      if (!length(lr$rows)) { addlog("merge", idof(l), "", lr$method, "already one record",
                                     "duplicate and master are the same row in the linkage input"); next }
      # fill blanks in the kept record from the duplicate, then drop the duplicate
      for (cc in setdiff(names(m), c("qc_corrected_fields","qc_merged"))) {
        if (!nzchar(blank(m[master, cc]))) { fillv <- blank(m[lr$rows, cc]); fillv <- fillv[nzchar(fillv)]
          if (length(fillv)) m[master, cc] <- fillv[1] } }
      drop_rows <- c(drop_rows, lr$rows); m$qc_merged[master] <- "Yes"
      addlog("merge", idof(l), "", lr$method, "applied", sprintf("merged into lab ID %s", uid[master]))
    }
  }
  n_merge_rows <- length(unique(drop_rows))
  if (n_merge_rows) m <- m[-unique(drop_rows), , drop = FALSE]

  ## 4. query status (audit only) ---------------------------------------------
  cs <- qc_rj(file.path(DASH_DATA, "cleaning_status.json")); if (is.null(cs)) cs <- list()
  cs_status <- table(vapply(cs, function(v) as.character(v$status %||% ""), character(1)))

  summary <- data.frame(check.names = FALSE,
    Item = c("Deletions listed", "  rows removed from linkage input",
             "Corrections listed (field values)", "  applied", "  not applied",
             "Duplicate merges listed (records)", "  duplicate rows removed", "  already a single record in this file",
             "  duplicate not in this file",
             paste0("Site queries: ", names(cs_status))),
    N = c(length(del), n_del,
          sum(log$type == "correction"), sum(log$type == "correction" & log$status == "applied"),
          sum(log$type == "correction" & log$status != "applied"),
          sum(log$type == "merge"), n_merge_rows, sum(log$type == "merge" & log$status == "already one record"),
          sum(log$type == "merge" & log$status == "not applied"), as.integer(cs_status)))
  list(m = m, log = log, summary = summary, n_del = n_del, n_merge = n_merge_rows)
}
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
