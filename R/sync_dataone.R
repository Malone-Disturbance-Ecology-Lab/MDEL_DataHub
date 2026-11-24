# sync_dataone.R
# ---------------------------------------------------------------------------
# Pull public metadata for the lab's data products from DataONE and save it to
# data/dataone_cache.json. The site build (R/build_pages.R) reads that cache,
# so the website always reflects what is archived in DataONE.
#
# What it does
#   1. Reads the collection query of the lab's DataONE portal (portal 3107251,
#      https://search.dataone.org/portals/3107251) so "the lab's datasets"
#      means the same thing here as on DataONE.
#   2. Fetches every current (non-obsoleted) metadata record matched by that
#      query, plus any record listed in data/catalog.yml.
#   3. Looks up DOIs for EDI / LTER packages from the EDI PASTA API.
#   4. Writes data/dataone_cache.json and prints a short report:
#        - datasets in DataONE that are not yet in catalog.yml (add or exclude)
#        - catalog datasets the portal query does not match (they will not show
#          on the DataONE portal page either)
#        - catalog datasets that have a newer version in DataONE
#
# Run from the project root:   Rscript R/sync_dataone.R
# Needs: jsonlite, yaml (install.packages(c("jsonlite", "yaml")))
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(jsonlite)
  library(yaml)
})

SOLR <- "https://cn.dataone.org/cn/v2/query/solr/"
PORTAL_SERIES_ID <- "urn:uuid:58c95f86-8417-4e05-b9b1-c62c96eff575"
# Used only if the portal document cannot be read.
FALLBACK_QUERY <- paste0(
  "((author:*Sparkle* AND authorLastName:*Malone*) OR ",
  "isPartOf:urn\\:uuid\\:58c95f86-8417-4e05-b9b1-c62c96eff575) ",
  "AND (-obsoletedBy:* AND formatType:METADATA)"
)
FIELDS <- c("id", "seriesId", "datasource", "title", "origin", "pubDate",
            "beginDate", "endDate", "keywords", "abstract", "attributeName",
            "westBoundCoord", "southBoundCoord", "eastBoundCoord",
            "northBoundCoord", "obsoletedBy")

cache_path   <- "data/dataone_cache.json"
catalog_path <- "data/catalog.yml"

solr <- function(q, fl = FIELDS, rows = 1000) {
  url <- paste0(SOLR, "?q=", URLencode(q, reserved = TRUE),
                "&fl=", paste(fl, collapse = ","),
                "&rows=", rows, "&wt=json")
  fromJSON(url, simplifyVector = FALSE)$response$docs
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
day <- function(x) if (is.null(x)) NULL else substr(x, 1, 10)

# --- 1. portal collection query ---------------------------------------------
portal_query <- tryCatch({
  q <- sprintf('seriesId:"%s" AND -obsoletedBy:*', PORTAL_SERIES_ID)
  solr(q, fl = c("id", "collectionQuery"))[[1]]$collectionQuery
}, error = function(e) NULL) %||% FALLBACK_QUERY
message("Portal query: ", portal_query)

# --- 2. fetch records --------------------------------------------------------
catalog  <- read_yaml(catalog_path)
products <- catalog$products
cat_ids  <- unlist(lapply(products, function(p) c(p$dataone_id, p$components)))
excl_ids <- vapply(catalog$excluded %||% list(), function(x) x$id, "")

portal_docs <- solr(portal_query)
portal_ids  <- vapply(portal_docs, function(d) d$id, "")

missing <- setdiff(cat_ids, portal_ids)
extra_docs <- list()
if (length(missing)) {
  q <- paste0("id:(", paste0('"', missing, '"', collapse = " OR "), ")")
  extra_docs <- solr(q)
}
docs <- c(portal_docs, extra_docs)

# --- 3. DOIs -----------------------------------------------------------------
old <- if (file.exists(cache_path)) fromJSON(cache_path, simplifyVector = FALSE)$records else list()
old_doi <- setNames(lapply(old, function(r) r$doi), vapply(old, function(r) r$id, ""))

pasta_doi <- function(id) {
  m <- regmatches(id, regexec("pasta\\.lternet\\.edu/package/metadata/eml/([^/]+)/(\\d+)/(\\d+)", id))[[1]]
  if (length(m) != 4) return(NULL)
  url <- sprintf("https://pasta.lternet.edu/package/doi/eml/%s/%s/%s", m[2], m[3], m[4])
  out <- tryCatch(readLines(url, warn = FALSE), error = function(e) NULL)
  if (is.null(out) || !length(out)) return(NULL)
  sub("^doi:", "", out[1])
}

find_doi <- function(d) {
  if (startsWith(d$id, "doi:")) return(sub("^doi:", "", d$id))
  sid <- d$seriesId %||% ""
  if (grepl("doi", sid)) return(sub("^(https?://doi.org/|doi:)", "", sid))
  pasta_doi(d$id) %||% old_doi[[d$id]]
}

# --- 4. build cache ----------------------------------------------------------
records <- lapply(docs, function(d) {
  list(
    id        = d$id,
    node      = d$datasource,
    title     = d$title,
    authors   = d$origin %||% list(),
    pubDate   = day(d$pubDate),
    begin     = day(d$beginDate),
    end       = day(d$endDate),
    bbox      = list(west = d$westBoundCoord, south = d$southBoundCoord,
                     east = d$eastBoundCoord, north = d$northBoundCoord),
    keywords  = d$keywords %||% list(),
    variables = head(unique(unlist(d$attributeName)), 30) %||% list(),
    abstract  = d$abstract %||% "",
    doi       = find_doi(d)
  )
})

write_json(
  list(retrieved = format(Sys.Date()),
       source = paste("DataONE CN Solr index; portal", PORTAL_SERIES_ID),
       records = records),
  cache_path, auto_unbox = TRUE, pretty = TRUE, null = "null"
)
message("Wrote ", length(records), " records to ", cache_path)

# --- 5. report ---------------------------------------------------------------
new_ids <- setdiff(portal_ids, c(cat_ids, excl_ids))
if (length(new_ids)) {
  message("\nIn DataONE but not in catalog.yml (add as a product or list under `excluded`):")
  for (d in portal_docs) if (d$id %in% new_ids) message("  - ", d$id, "\n      ", d$title)
}
not_in_portal <- setdiff(vapply(products, function(p) p$dataone_id %||% NA_character_, ""), c(portal_ids, NA))
if (length(not_in_portal)) {
  message("\nIn catalog.yml but NOT matched by the DataONE portal query ",
          "(these do not appear on the portal page either):")
  message(paste0("  - ", not_in_portal, collapse = "\n"))
}
newer <- Filter(function(d) !is.null(d$obsoletedBy), extra_docs)
if (length(newer)) {
  message("\nCatalog entries with a newer version in DataONE (update dataone_id):")
  for (d in newer) message("  - ", d$id, " -> ", d$obsoletedBy)
}
