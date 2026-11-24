# MDEL DataHub

The **Malone Disturbance Ecology Lab – Open Data & Tools** site: a branded front end for browsing the lab's data products, software, interactive apps and publications. It does not replace DataONE. Every product stays archived in DataONE (via EDI, KNB and other member repositories) with its DOI, version and license; this site makes those products easier to find, understand, use and cite.

## How it works

```
data/catalog.yml  ──┐   (curated by the lab)
                     ├─► R/build_pages.R ─► products/*.qmd, collections/*.qmd ─► Quarto ─► _site/
DataONE ─► R/sync_dataone.R ─► data/dataone_cache.json
```

| File | What it is | Who edits it |
|---|---|---|
| `data/catalog.yml` | Which datasets appear, their collection, type, plain-language summary, applications, limitations, license | Lab members |
| `data/collections.yml` | The four research collections | Lab members |
| `data/dataone_cache.json` | Titles, authors, coverage, variables, abstracts, DOIs from DataONE | `R/sync_dataone.R` |
| `R/build_pages.R` | Generates one landing page per product with schema.org `Dataset` JSON-LD | Runs automatically before each render |
| `REVIEW.md` | Checklist of draft fields that still need checking | Generated |
| `.github/workflows/publish.yml` | Weekly sync + render + publish to GitHub Pages | – |

## Run it locally

```r
install.packages(c("jsonlite", "yaml"))
```

```bash
Rscript R/sync_dataone.R   # refresh metadata from DataONE (needs internet)
quarto preview             # build pages and open the site
```

`sync_dataone.R` also prints datasets that are in DataONE but not yet in the catalog, and catalog datasets the DataONE portal query does not pick up.

## Add a data product

1. Archive it in DataONE / EDI with a descriptive, searchable title.
2. Run `Rscript R/sync_dataone.R`; the new dataset is listed in the report.
3. Add an entry to `data/catalog.yml` (copy an existing one) and render.

## Status (first version, Nov 2026)

- 13 archived products plus 1 forthcoming placeholder (Global Mangrove Dynamics).
- Fields drafted without checking the data are flagged on each page and listed in `REVIEW.md`.
- Software, Apps and Publications pages are placeholders.
