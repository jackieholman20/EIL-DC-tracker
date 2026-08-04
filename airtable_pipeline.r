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

# ---- fetch all records from a table, handling pagination ----
# cell_format: "string" (flattens lookups/rich text) or "json" (keeps link
# fields as record ID arrays, needed for joins)
fetch_all_records <- function(base_id, table_id, api_key, cell_format = "json") {
  records <- list()
  offset <- NULL
  repeat {
    query <- list(pageSize = 100)
    if (cell_format == "string") {
      query$cellFormat <- "string"
      query$timeZone   <- "America/Chicago"   # required when cellFormat="string"
      query$userLocale  <- "en-us"             # required when cellFormat="string"
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

# ---- helpers ----
get_field <- function(rec, field, default = "") {
  val <- rec$fields[[field]]
  if (is.null(val)) return(default)
  val
}

make_id <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]+", "-") %>%
    str_replace_all("^-|-$", "") %>%
    str_trunc(60, ellipsis = "")
}

format_date <- function(iso_date) {
  d <- suppressWarnings(as.Date(iso_date))
  if (is.na(d)) return("")
  format(d, "%B %d, %Y")
}

# ---- build a case_record_id -> list(documents) lookup ----
docs_by_case <- list()
for (doc in docs_raw) {
  case_ids <- get_field(doc, "Cases", list())   # array of linked Cases record IDs
  if (length(case_ids) == 0) next
  
  doc_entry <- list(
    title         = get_field(doc, "Name"),
    filingDisplay = format_date(get_field(doc, "Filing Date")),
    link          = get_field(doc, "Link to Filing"),
    description   = get_field(doc, "Description")
  )
  
  for (cid in case_ids) {
    docs_by_case[[cid]] <- c(docs_by_case[[cid]], list(doc_entry))
  }
}

# ---- build the case records ----
cases <- lapply(cases_raw, function(rec) {
  
  # With cellFormat="string", a checked box comes back as text (e.g. "checked"),
  # not boolean TRUE - and an unchecked box is omitted from `fields` entirely
  # (Airtable drops "empty" values like false). So presence of the key is the
  # reliable signal, not its value.
  if (is.null(rec$fields[["Published"]])) return(NULL)
  
  case_name <- get_field(rec, "Case Name")
  case_docs <- docs_by_case[[rec$id]]
  if (is.null(case_docs)) case_docs <- list()
  
  list(
    id                 = make_id(case_name),
    shortName          = get_field(rec, "Online Display Name", case_name),
    citation           = case_name,
    docket             = get_field(rec, "Docket Number"),
    filingDisplay      = get_field(rec, "Filing Date"),   # already string-formatted by Airtable
    yearFiled          = str_extract(get_field(rec, "Filing Date"), "\\d{4}"),
    location           = get_field(rec, "Name (from Jurisdiction)"),
    forum              = get_field(rec, "Name (from Courts)"),
    forumType          = get_field(rec, "Type (from Jurisdiction)"),
    caseLevel          = get_field(rec, "Level (from Courts)"),
    developerPlaintiff = get_field(rec, "Developer Plaintiff"),
    corporateEntities  = get_field(rec, "Corporate Entities"),
    category           = get_field(rec, "Name (from Category)"),
    docketLink         = get_field(rec, "Link to Docket"),
    status             = get_field(rec, "Status"),
    summary            = get_field(rec, "At Issue"),
    documents          = case_docs
  )
})

cases <- Filter(Negate(is.null), cases)

write_json(cases, "cases_data.json", auto_unbox = TRUE, pretty = TRUE, na = "null")
cat("Wrote", length(cases), "published cases (with documents) to cases_data.json\n")