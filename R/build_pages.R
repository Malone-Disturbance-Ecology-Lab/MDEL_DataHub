# build_pages.R
# ---------------------------------------------------------------------------
# Generates the product and collection pages of the MDEL DataHub from
#   data/catalog.yml          (what the lab curates)
#   data/collections.yml      (research collections)
#   data/dataone_cache.json   (what DataONE says; refreshed by sync_dataone.R)
#
# Quarto runs this automatically before every render (see `pre-render` in
# _quarto.yml). You can also run it by hand: Rscript R/build_pages.R
#
# Outputs (all generated; do not edit by hand):
#   products/<slug>.qmd        one landing page per data product
#   collections/<id>.qmd       one page per collection
#   _generated/collection-cards.md, _generated/catalog-jsonld.html
#   REVIEW.md                  checklist of draft fields to confirm
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(jsonlite)
  library(yaml)
})

SITE_URL <- "https://malone-disturbance-ecology-lab.github.io/MDEL_DataHub"  # keep in sync with _quarto.yml
LAB      <- "Malone Disturbance Ecology Lab"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

# Publications and names contain accents; make sure files are read as UTF-8.
invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")))
invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "en_US.UTF-8")))

catalog     <- read_yaml("data/catalog.yml")
pubs        <- if (file.exists("data/publications.yml")) read_yaml("data/publications.yml") else list(publications = list())

# Interactive apps referenced from catalog.yml (related_apps).
APPS <- list(
  rustmapper = list(name = "RustMapper app", url = "https://angelchen.shinyapps.io/WPBR/"),
  `enp-fires` = list(name = "Everglades fire history app", url = "https://angelchen.shinyapps.io/ENP_fires/")
)

scholar_url <- function(id) paste0("https://scholar.google.com/citations?view_op=view_citation&hl=en&user=abE9cVgAAAAJ&citation_for_view=abE9cVgAAAAJ:", id)
pub_line <- function(x) {
  a <- gsub("\\b(SL Malone|S Malone)\\b", "**\\1**", x$authors %||% "", perl = TRUE)
  sprintf("- %s (%s). [%s](%s). _%s_.", a, x$year, x$title, scholar_url(x$id), x$venue %||% "")
}
products    <- catalog$products
collections <- read_yaml("data/collections.yml")
coll_by_id  <- setNames(collections, vapply(collections, `[[`, "", "id"))
cache       <- fromJSON("data/dataone_cache.json", simplifyVector = FALSE)$records
rec_by_id   <- setNames(cache, vapply(cache, `[[`, "", "id"))

for (d in c("products", "collections", "_generated")) dir.create(d, showWarnings = FALSE)
unlink(list.files("products", pattern = "\\.qmd$", full.names = TRUE))
unlink(list.files("collections", pattern = "\\.qmd$", full.names = TRUE))

# --- helpers -----------------------------------------------------------------
repo_name <- function(node) switch(node %||% "",
  "urn:node:EDI"  = "Environmental Data Initiative (EDI)",
  "urn:node:LTER" = "Environmental Data Initiative (EDI), FCE LTER",
  "urn:node:KNB"  = "Knowledge Network for Biocomplexity (KNB)",
  "urn:node:DRYAD" = "Dryad",
  "DataONE")

pasta_parts <- function(id) {
  m <- regmatches(id, regexec("package/metadata/eml/([^/]+)/(\\d+)/(\\d+)", id))[[1]]
  if (length(m) == 4) m[2:4] else NULL
}

archive_url <- function(id, node) {
  pp <- pasta_parts(id)
  if (!is.null(pp))
    return(sprintf("https://portal.edirepository.org/nis/mapbrowse?scope=%s&identifier=%s&revision=%s", pp[1], pp[2], pp[3]))
  if (identical(node, "urn:node:KNB"))
    return(paste0("https://knb.ecoinformatics.org/view/", URLencode(id, reserved = TRUE)))
  dataone_url(id)
}
dataone_url <- function(id) paste0("https://search.dataone.org/view/", URLencode(id, reserved = TRUE))

fmt_authors <- function(a) {
  a <- unlist(a)
  if (length(a) <= 1) return(a %||% LAB)
  if (length(a) == 2) return(paste(a, collapse = " and "))
  paste0(paste(a[-length(a)], collapse = ", "), ", and ", a[length(a)])
}

citation <- function(r) {
  year <- substr(r$pubDate %||% "", 1, 4)
  link <- if (!is.null(r$doi)) paste0("https://doi.org/", r$doi) else archive_url(r$id, r$node)
  sprintf("%s (%s). %s. %s. %s", fmt_authors(r$authors), year %||% "n.d.",
          r$title, repo_name(r$node), link)
}

bullets <- function(x) if (length(x)) paste0("- ", unlist(x), collapse = "\n") else "_Not yet documented._"
mark    <- function(field, guesses) if (field %in% guesses) " [needs review]{.badge .text-bg-warning}" else ""

person_or_org <- function(n) {
  if (grepl("Station|Service|Program|Center|Lab|University", n)) list(`@type` = "Organization", name = n)
  else list(`@type` = "Person", name = n)
}

jsonld_dataset <- function(p, r, url) {
  bb <- r$bbox
  spatial <- NULL
  if (!is.null(bb$west)) {
    spatial <- list(`@type` = "Place", geo = if (bb$west == bb$east && bb$south == bb$north)
      list(`@type` = "GeoCoordinates", latitude = bb$south, longitude = bb$west)
    else list(`@type` = "GeoShape", box = sprintf("%s %s %s %s", bb$south, bb$west, bb$north, bb$east)))
  }
  x <- list(
    `@context` = "https://schema.org/",
    `@type` = "Dataset",
    name = r$title,
    description = paste(trimws(p$summary), r$abstract %||% ""),
    url = url,
    sameAs = c(archive_url(r$id, r$node), dataone_url(r$id)),
    identifier = if (!is.null(r$doi)) paste0("https://doi.org/", r$doi) else r$id,
    creator = lapply(unlist(r$authors), person_or_org),
    datePublished = r$pubDate,
    keywords = unlist(r$keywords),
    variableMeasured = unlist(r$variables),
    temporalCoverage = if (!is.null(r$begin)) paste0(r$begin, "/", r$end %||% "..") else NULL,
    spatialCoverage = spatial,
    license = license_url(p$license),
    isAccessibleForFree = TRUE,
    publisher = list(`@type` = "Organization", name = repo_name(r$node)),
    includedInDataCatalog = list(`@type` = "DataCatalog", name = paste(LAB, "DataHub"), url = SITE_URL)
  )
  x <- Filter(Negate(is.null), x)
  toJSON(x, auto_unbox = TRUE, pretty = TRUE)
}

license_url <- function(l) switch(l %||% "",
  "CC BY 4.0" = "https://creativecommons.org/licenses/by/4.0/",
  "CC0" = , "CC0 1.0" = "https://creativecommons.org/publicdomain/zero/1.0/",
  NULL)

code_r <- function(r) {
  pp <- pasta_parts(r$id)
  if (!is.null(pp)) {
    pkg <- paste(pp, collapse = ".")
    return(sprintf(
'```r
# install.packages("EDIutils")
library(EDIutils)

pkg <- "%s"
entities <- read_data_entity_names(pkg)   # list the files in the package
entities

# read the first entity into R
raw <- read_data_entity(pkg, entities$entityId[1])
dat <- readr::read_csv(raw)
```', pkg))
  }
  sprintf(
'```r
# install.packages("dataone")
library(dataone)

d1c <- D1Client("PROD", "%s")
pkg <- getDataPackage(d1c, identifier = "%s", lazyLoad = TRUE, quiet = FALSE)
getIdentifiers(pkg)   # files in the package
```', r$node %||% "urn:node:KNB", r$id)
}

code_py <- function(r) {
  pp <- pasta_parts(r$id)
  if (!is.null(pp)) {
    return(sprintf(
'```python
import io, requests, pandas as pd

pkg = "%s/%s/%s"
base = "https://pasta.lternet.edu/package"
ids = requests.get(f"{base}/data/eml/{pkg}").text.split()
names = requests.get(f"{base}/name/eml/{pkg}").text
print(names)

csv = requests.get(ids[0]).content
df = pd.read_csv(io.BytesIO(csv))
```', pp[1], pp[2], pp[3]))
  }
  sprintf(
'```python
import requests

# DataONE package listing (resource map) for this dataset
url = "https://cn.dataone.org/cn/v2/query/solr/"
q = {"q": \'resourceMap:"%s" OR id:"%s"\', "fl": "id,fileName,formatId", "wt": "json"}
print(requests.get(url, params=q).json()["response"]["docs"])
```', r$id, r$id)
}

yaml_front <- function(x) paste0("---\n", as.yaml(x, line.sep = "\n"), "---\n")

# --- product pages -----------------------------------------------------------
review <- c("# Review checklist", "",
  "Generated by `R/build_pages.R`. Each item is a draft field in `data/catalog.yml`",
  "that was written without checking the data. Confirm or fix it, then remove the",
  "field name from that product's `guesses:` list.", "")

for (p in products) {
  coll    <- coll_by_id[[p$collection]]
  status  <- p$status %||% "published"
  guesses <- unlist(p$guesses)
  r <- if (!is.null(p$dataone_id)) rec_by_id[[p$dataone_id]] else NULL
  if (!is.null(p$dataone_id) && is.null(r))
    warning("No DataONE record cached for ", p$slug, "; run R/sync_dataone.R")

  title <- p$title %||% r$title %||% p$slug
  url   <- sprintf("%s/products/%s.html", SITE_URL, p$slug)
  if (length(guesses))
    review <- c(review, sprintf("- [ ] **%s** (`%s`): %s", title, p$slug, paste(guesses, collapse = ", ")))

  front <- list(
    title = title,
    description = trimws(p$summary),
    date = r$pubDate,
    categories = c(p$type, coll$name),
    collection = p$collection,
    product_type = p$type,
    featured = if (isTRUE(p$featured)) "yes" else "no",
    status = status,
    `title-block-banner` = FALSE
  )
  if (status == "forthcoming") front$categories <- c(front$categories, "Forthcoming")
  front <- Filter(Negate(is.null), front)

  head <- c('<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.9.4/leaflet.min.css">',
            '<script src="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.9.4/leaflet.min.js"></script>')
  if (!is.null(r)) head <- c(head, '<script type="application/ld+json">', jsonld_dataset(p, r, url), "</script>")
  front$`include-in-header` <- list(text = paste(head, collapse = "\n"))

  body <- c(
    sprintf('[%s](../collections/%s.qmd){.collection-pill style="--c:%s"} [%s]{.type-pill}',
            coll$name, coll$id, coll$color, p$type),
    "",
    if (length(guesses)) c(
      '::: {.callout-warning appearance="simple"}',
      sprintf("**Draft page.** These fields were drafted and still need review: %s.", paste(guesses, collapse = ", ")),
      ":::", ""),
    ""
  )

  if (status == "forthcoming" || is.null(r)) {
    body <- c(body,
      "::: {.callout-note}",
      "This product is in preparation. It will be archived in DataONE with a DOI, and this page will then fill in automatically.",
      ":::", "",
      sprintf("[Explore the %s collection](../collections/%s.qmd){.btn .btn-outline-primary}", coll$name, coll$id))
  } else {
    bb <- r$bbox
    doi_md <- if (!is.null(r$doi)) sprintf("[%s](https://doi.org/%s)", r$doi, r$doi) else "_DOI to be added_ [needs review]{.badge .text-bg-warning}"
    facts <- c(
      "| | |", "|---|---|",
      sprintf("| **Time span** | %s to %s |", substr(r$begin %||% "?", 1, 4), substr(r$end %||% "?", 1, 4)),
      sprintf("| **Spatial extent** | %s |", if (is.null(bb$west)) "n/a" else sprintf("%.2f to %.2f lat, %.2f to %.2f lon", bb$south, bb$north, bb$west, bb$east)),
      sprintf("| **Resolution** | %s%s |", p$resolution %||% "_to be added_", mark("resolution", guesses)),
      sprintf("| **Archive** | [%s](%s) |", repo_name(r$node), archive_url(r$id, r$node)),
      sprintf("| **DOI** | %s |", doi_md),
      sprintf("| **License** | %s%s |", p$license %||% "_to be added_", mark("license", guesses)),
      sprintf("| **Published** | %s |", r$pubDate %||% "?")
    )
    map_js <- if (!is.null(bb$west)) sprintf(
'```{=html}
<div id="map" class="product-map"></div>
<script>
document.addEventListener("DOMContentLoaded", function () {
  var b = [[%f, %f], [%f, %f]];
  var map = L.map("map", {scrollWheelZoom: false});
  L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
    {attribution: "&copy; OpenStreetMap contributors"}).addTo(map);
  if (b[0][0] === b[1][0] && b[0][1] === b[1][1]) {
    L.circleMarker(b[0], {radius: 8, color: "%s"}).addTo(map); map.setView(b[0], 9);
  } else {
    L.rectangle(b, {color: "%s", weight: 2, fillOpacity: 0.15}).addTo(map); map.fitBounds(b, {padding: [20, 20]});
  }
});
</script>
```', bb$south, bb$west, bb$north, bb$east, coll$color, coll$color) else ""

    rel <- c()
    if (length(p$components)) {
      rel <- c(rel, "**Individual layers**", "")
      for (cid in unlist(p$components)) {
        cr <- rec_by_id[[cid]]
        rel <- c(rel, sprintf("- [%s](%s)", cr$title %||% cid, if (!is.null(cr$doi)) paste0("https://doi.org/", cr$doi) else dataone_url(cid)))
      }
      rel <- c(rel, "")
    }
    if (length(p$previous_versions))
      rel <- c(rel, "**Earlier versions**", "", paste0("- [", unlist(p$previous_versions), "](", vapply(unlist(p$previous_versions), dataone_url, ""), ")"), "")
    if (length(p$related_apps))
      rel <- c(rel, "**Apps**", "", vapply(unlist(p$related_apps), function(a) sprintf("- [%s](%s)", APPS[[a]]$name %||% a, APPS[[a]]$url %||% "../apps.qmd"), ""), "")
    pp <- Filter(function(x) identical(x$product, p$slug), pubs$publications)
    if (length(pp))
      rel <- c(rel, "**Papers**", "", vapply(pp, pub_line, ""), "")
    rel <- c(rel, sprintf("**Collection:** [%s](../collections/%s.qmd)", coll$name, coll$id))

    body <- c(body,
      ":::: {.grid}", "::: {.g-col-12 .g-col-md-6}", map_js, ":::",
      "::: {.g-col-12 .g-col-md-6}", facts, ":::", "::::", "",
      sprintf("[Download from %s](%s){.btn .btn-primary} [View in DataONE](%s){.btn .btn-outline-primary}",
              sub(" \\(.*", "", repo_name(r$node)), archive_url(r$id, r$node), dataone_url(r$id)), "",
      "## About this dataset", "", r$abstract %||% "", "",
      "## Variables", "",
      if (length(r$variables)) paste0("`", unlist(r$variables), "`", collapse = " · ") else "_See the data package metadata for the full list of variables, units and uncertainty._",
      "", "_Units, definitions and uncertainty are documented in the archived metadata (EML)._", "",
      paste0("## Research applications", mark("applications", guesses)), "", bullets(p$applications), "",
      paste0("## Known limitations", mark("limitations", guesses)), "", bullets(p$limitations), "",
      "## How to cite", "",
      "::: {.citation-box}", citation(r), ":::", "",
      "## Get the data with code", "",
      "::: {.panel-tabset}", "### R", "", code_r(r), "", "### Python", "", code_py(r), "",
      "### Earth Engine", "", "_Earth Engine examples are planned for geospatial products._", ":::", "",
      "## Related", "", rel
    )
  }

  writeLines(c(yaml_front(front), "", body), file.path("products", paste0(p$slug, ".qmd")))
}

writeLines(review, "REVIEW.md")

# --- collection pages and cards ----------------------------------------------
n_in <- function(cid) sum(vapply(products, function(p) identical(p$collection, cid), TRUE))
cards <- c('::: {.collection-grid}')
for (cl in collections) {
  n <- n_in(cl$id)
  cards <- c(cards, sprintf(
'::: {.collection-card style="--c:%s;%s"}
[%s](collections/%s.qmd){.stretched-link}

%s

[%d product%s]{.count}
:::', cl$color,
    if (is.null(cl$image)) "" else sprintf(" background-image:linear-gradient(180deg,%s22 0%%,%sF2 70%%),url(%s)", cl$color, cl$color, cl$image),
    cl$name, cl$id, cl$tagline, n, if (n == 1) "" else "s"))
  page <- c(
    yaml_front(list(
      title = cl$name, subtitle = cl$tagline,
      `title-block-banner` = if (!is.null(cl$image)) paste0("../", cl$image) else cl$color,
      `title-block-banner-color` = "white",
      `title-block-style` = "default",
      listing = list(list(
        id = "coll", contents = "../products/*.qmd",
        include = list(collection = cl$id),
        type = "grid", `grid-columns` = 3, sort = "date desc",
        fields = list("title", "description", "categories", "date"),
        `date-format` = "YYYY", `image-placeholder` = "../assets/placeholder.svg"
      )))),
    "", trimws(cl$description), "", ":::{#coll}", ":::", "",
    if (!is.null(cl$credit)) sprintf("[Banner photo: [%s](%s)]{.photo-credit}", cl$credit, cl$credit_url) else "")
  writeLines(page, file.path("collections", paste0(cl$id, ".qmd")))
}
writeLines(c(cards, ":::"), "_generated/collection-cards.md")

# --- publications ------------------------------------------------------------
pl <- pubs$publications
linked <- Filter(function(x) !is.null(x$product), pl)
out <- c(sprintf("_%d publications from [Google Scholar](%s), retrieved %s. Sparkle Malone in bold. Conference abstracts and grant records are not listed._",
                 length(pl), pubs$source %||% "", pubs$retrieved %||% ""), "",
         "## Papers and datasets behind DataHub products", "")
for (x in linked) out <- c(out, sub("\\.$", sprintf(". → [data product](products/%s.qmd)", x$product), pub_line(x)))
type_lab <- c(article = "", data = " `dataset`", preprint = " `preprint`", chapter = " `chapter`", thesis = " `thesis`")
out <- c(out, "", "## All publications", "")
for (yr in sort(unique(vapply(pl, function(x) as.integer(x$year), 0L)), decreasing = TRUE)) {
  out <- c(out, paste("###", yr), "")
  for (x in Filter(function(x) as.integer(x$year) == yr, pl)) out <- c(out, paste0(pub_line(x), type_lab[[x$type %||% "article"]]))
  out <- c(out, "")
}
writeLines(out, "_generated/publications.md", useBytes = TRUE)

# --- DataCatalog JSON-LD for the products page --------------------------------
cat_ld <- list(
  `@context` = "https://schema.org/", `@type` = "DataCatalog",
  name = paste(LAB, "DataHub"),
  description = "Data products from the Malone Disturbance Ecology Lab, archived through DataONE.",
  url = paste0(SITE_URL, "/products.html"),
  creator = list(`@type` = "Organization", name = LAB),
  dataset = lapply(Filter(function(p) !is.null(p$dataone_id), products), function(p) {
    r <- rec_by_id[[p$dataone_id]]
    list(`@type` = "Dataset", name = r$title %||% p$slug,
         url = sprintf("%s/products/%s.html", SITE_URL, p$slug))
  })
)
writeLines(c('<script type="application/ld+json">', toJSON(cat_ld, auto_unbox = TRUE, pretty = TRUE), "</script>"),
           "_generated/catalog-jsonld.html")

message("Built ", length(products), " product pages and ", length(collections), " collection pages.")
