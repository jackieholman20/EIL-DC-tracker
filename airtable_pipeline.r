# generate_cases_data.R
#
# Pulls case + document data from the real DCLT Airtable base and writes
# cases_data.json in the schema data-center-litigation-tracker-connected.html
# expects.
#
# Cases are fetched with cellFormat="string" so Lookup fields and the rich-text
# "At Issue" field come back as plain strings. Documents are fetched with the
# DEFAULT (json) cellFormat instead, because we need its "Cases" link field to
# stay as real record IDs (string format would replace them with the linked
# record's display text, which breaks the join). Record IDs (rec$id) are
# always present regardless of cellFormat, since cellFormat only affects the
# values inside rec$fields.

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)


BASE_ID     <- "appTKw9dQk3Lh9EZQ"
CASES_TABLE <- "tblV3mKGLqdm3B8R7"
DOCS_TABLE  <- "tblwAeu84P5Xu3RG5"
API_KEY     <- Sys.getenv("AIRTABLE_API_KEY")

if (API_KEY == "") stop("AIRTABLE_API_KEY environment variable is not set.")

# ---------------------------------------------------------------------------
# Website field  ->  Airtable column(s) on the Cases table (first match wins)
# ---------------------------------------------------------------------------
COLUMNS <- list(
  caseName           = c("Case Name"),
  shortName          = c("Online Display Name"),
  docket             = c("Docket Number"),
  filingDate         = c("Filing Date"),
  status             = c("Status"),
  location           = c("Location", "Name (from Jurisdiction)"),
  forum              = c("Forum", "Name (from Courts)"),
  forumType          = c("Type", "Type (from Jurisdiction)"),
  caseLevel          = c("Level (from Courts)"),
  developerPlaintiff = c("Developer Plaintiff"),
  corporateEntities  = c("Corporate Entities"),
  docketLink         = c("Link to Docket"),
  media              = c("Link to Media"),
  published          = c("Published"),
  lawType            = c("Statutory, Constitutional Law, or Common Law"),
  category           = c("Category", "Name (from Category)"),
  secondaryCategory  = c("Secondary Category", "Name (from Secondary Category)"),
  summary            = c("At Issue"),
  lastUpdated        = c("Date of Last Update")
)

# Fields the site cannot work without - the script stops if these are missing.
REQUIRED <- c("caseName", "published")

# ---- fetch all records from a table, handling pagination ----
fetch_all_records <- function(base_id, table_id, api_key, cell_format = "json") {
  records <- list()
  offset <- NULL
  repeat {
    query <- list(pageSize = 100)
    if (cell_format == "string") {
      query$cellFormat <- "string"
      query$timeZone   <- "America/Chicago"   # required when cellFormat="string"
      query$userLocale <- "en-us"             # required when cellFormat="string"
    }
    if (!is.null(offset)) query$offset <- offset

    resp <- GET(
      url = paste0("https://api.airtable.com/v0/", base_id, "/", table_id),
      add_headers(Authorization = paste("Bearer", api_key)),
      query = query
    )
    stop_for_status(resp)
    parsed <- content(resp, as = "parsed", type = "application/json")
    records <- c(records, parsed$records)

    if (is.null(parsed$offset)) break
    offset <- parsed$offset
  }
  records
}

cases_raw <- fetch_all_records(BASE_ID, CASES_TABLE, API_KEY, cell_format = "string")
docs_raw  <- fetch_all_records(BASE_ID, DOCS_TABLE,  API_KEY, cell_format = "json")
cat("Fetched", length(cases_raw), "case records and", length(docs_raw), "document records\n")

# ---- resolve which column each website field reads from ----
# (Airtable omits empty cells, so a column only "exists" if some record has it.)
all_names <- unique(unlist(lapply(cases_raw, function(r) names(r$fields))))

col <- lapply(COLUMNS, function(cands) {
  hit <- intersect(cands, all_names)
  if (length(hit) > 0) hit[1] else NULL
})

cat("\nColumn mapping:\n")
for (k in names(COLUMNS)) {
  if (is.null(col[[k]])) {
    cat(sprintf("  %-20s NOT FOUND (looked for: %s)\n", k, paste(COLUMNS[[k]], collapse = " | ")))
  } else {
    cat(sprintf("  %-20s <- \"%s\"\n", k, col[[k]]))
  }
}
missing_required <- REQUIRED[vapply(REQUIRED, function(k) is.null(col[[k]]), logical(1))]
if (length(missing_required) > 0) {
  stop("Required column(s) not found on the Cases table: ",
       paste(missing_required, collapse = ", "),
       "\nColumns seen: ", paste(sort(all_names), collapse = " | "))
}
cat("\n")

# ---- helpers ----
# value of a website field for a record ("" if the column or cell is empty)
val <- function(rec, key, default = "") {
  name <- col[[key]]
  if (is.null(name)) return(default)
  v <- rec$fields[[name]]
  if (is.null(v)) return(default)
  v
}

# plain field lookup by Airtable column name (used for the Documents table)
get_field <- function(rec, field, default = "") {
  v <- rec$fields[[field]]
  if (is.null(v)) return(default)
  v
}

make_id <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]+", "-") %>%
    str_replace_all("^-|-$", "") %>%
    str_trunc(60, ellipsis = "")
}

# "March 05, 2026" -> "March 5, 2026" (drop the zero padding)
tidy_date <- function(d) str_replace(format(d, "%B %d, %Y"), " 0(\\d),", " \\1,")

# Any date style Airtable can hand back (2026-03-17, 3/17/2026,
# "March 17, 2026") -> "March 17, 2026". Unrecognised text is passed through
# unchanged so nothing silently disappears.
format_any_date <- function(x) {
  x <- as.character(x)
  if (length(x) == 0 || is.na(x[1]) || x[1] == "") return("")
  x <- x[1]
  d <- NA
  iso  <- str_extract(x, "\\d{4}-\\d{2}-\\d{2}")
  long <- str_extract(x, "[A-Za-z]+ \\d{1,2}, \\d{4}")
  us   <- str_extract(x, "\\d{1,2}/\\d{1,2}/\\d{4}")
  if (!is.na(iso))                 d <- suppressWarnings(as.Date(iso))
  if (is.na(d) && !is.na(long))    d <- suppressWarnings(as.Date(long, format = "%B %d, %Y"))
  if (is.na(d) && !is.na(us))      d <- suppressWarnings(as.Date(us, format = "%m/%d/%Y"))
  if (is.na(d)) return(x)
  tidy_date(d)
}

# every http(s) URL in a cell (separated by ; , spaces or newlines) -> list
extract_urls <- function(x) {
  if (is.null(x) || length(x) == 0) return(list())
  txt  <- paste(unlist(x), collapse = "\n")
  urls <- str_extract_all(txt, "https?://[^\\s,;<>\"')]+")[[1]]
  urls <- unique(str_replace(urls, "[.]+$", ""))
  as.list(urls)
}

# ---- build a case_record_id -> list(documents) lookup ----
docs_by_case <- list()
for (doc in docs_raw) {
  case_ids <- get_field(doc, "Cases", list())   # array of linked Cases record IDs
  if (length(case_ids) == 0) next

  doc_entry <- list(
    title         = get_field(doc, "Name"),
    filingDisplay = format_any_date(get_field(doc, "Filing Date")),
    link          = get_field(doc, "Link to Filing"),
    description   = get_field(doc, "Description")
  )

  for (cid in case_ids) {
    docs_by_case[[cid]] <- c(docs_by_case[[cid]], list(doc_entry))
  }
}

# ---- build the case records ----
cases <- lapply(cases_raw, function(rec) {

  # Only cases with the Published box checked go on the site. With
  # cellFormat="string" a checked box arrives as text ("checked") and an
  # unchecked box is omitted entirely, so presence of the key is the signal.
  if (is.null(rec$fields[[col$published]])) return(NULL)

  case_name <- val(rec, "caseName")
  case_docs <- docs_by_case[[rec$id]]
  if (is.null(case_docs)) case_docs <- list()

  filing <- format_any_date(val(rec, "filingDate"))

  list(
    id                 = make_id(case_name),
    shortName          = val(rec, "shortName", case_name),
    citation           = case_name,
    docket             = val(rec, "docket"),
    filingDisplay      = filing,
    yearFiled          = str_extract(filing, "\\d{4}"),
    location           = val(rec, "location"),
    forum              = val(rec, "forum"),
    forumType          = val(rec, "forumType"),
    caseLevel          = val(rec, "caseLevel"),
    developerPlaintiff = val(rec, "developerPlaintiff"),
    corporateEntities  = val(rec, "corporateEntities"),
    category           = val(rec, "category"),
    docketLink         = val(rec, "docketLink"),
    status             = val(rec, "status"),
    summary            = val(rec, "summary"),
    documents          = case_docs,
    lawType            = val(rec, "lawType"),
    secondaryCategory  = val(rec, "secondaryCategory"),
    lastUpdated        = format_any_date(val(rec, "lastUpdated")),
    media              = extract_urls(val(rec, "media", NULL))
  )
})

cases <- Filter(Negate(is.null), cases)

write_json(cases, "cases_data.json", auto_unbox = TRUE, pretty = TRUE, na = "null")
cat("Wrote", length(cases), "published cases to cases_data.json\n")

# ---- fill-rate report: a 0 here usually means a wrong column name ----
filled <- function(key) sum(vapply(cases, function(c) {
  v <- c[[key]]
  length(v) > 0 && !all(is.na(v)) && !identical(v, "")
}, logical(1)))
cat("\nFill rate (cases with a value):\n")
for (k in c("shortName", "docket", "filingDisplay", "location", "forum", "forumType",
            "caseLevel", "category", "secondaryCategory", "lawType", "status",
            "summary", "lastUpdated", "media", "documents")) {
  cat(sprintf("  %-18s %d / %d\n", k, filled(k), length(cases)))
}
