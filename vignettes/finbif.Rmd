---
title: "Introduction to the finbif package"
author: "William K Morris"
output: 
  rmarkdown::html_vignette:
    toc: true
vignette: >
  %\VignetteIndexEntry{1. Introduction to the finbif package}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---


The Finnish Biodiversity Information Facility (FinBIF) aggregates Finnish
biodiversity data from multiple sources in a single open access portal for
researchers, citizen scientists, industry and government. The `finbif` R
package provides access to the FinBIF API directly from within R. FinBIF allows
users of biodiversity information to find, access, combine and visualise data on
Finnish plants, animals and microorganisms. The finbif R package makes the
publicly available data in FinBIF accessible from within R. Biodiversity
information is available on taxonomy and taxon occurrence. Occurrence data can
be filtered by taxon, time, location and other variables. The data accessed are
conveniently preformatted for subsequent analyses.

## Installing the finbif package
You can install the development version of finbif from
[GitHub](https://github.com),

```r
remotes::install_github("luomus/finbif")
```

## Loading the finbif package

```r
library(finbif)
```

## Getting a FinBIF access token
To use the FinBIF API you must first request and set a personal access token.
You can request an API token to be sent to your email address with the function
`finbif_get_token`.

```r
finbif_request_token("your@email.com")
```

Copy the access token that was sent to your email and set it as the environment
variable `FINBIF_ACCESS_TOKEN` either for the current session,

```r
Sys.setenv(
  FINBIF_ACCESS_TOKEN = "xtmSOIxjPwq0pOMB1WvcZgFLU9QBklauOlonWl8K5oaLIx8RniJLrvcJU4v9H7Et"
)
# Note: the above is not a real access token. Do not try using it.
```
, or by adding it to a `Renviron` startup file (see
[here](https://rviews.rstudio.com/2017/04/19/r-for-enterprise-understanding-r-s-startup/)
for details).

## Working with taxa
You can check to see if a taxon exists in the FinBIF database.

```r
finbif_check_taxa("Ursus arctos")
#> [Ursus arctos] ID: MX.47348
```

If the taxon is the FinBIF database its unique ID is returned. When a taxon is
not in the FinBIF database it is reported as "not found" and for that taxa the
list element is `NA`.

```r
(taxa <- finbif_check_taxa(c("Ursus arctos", "Moomin")))
#> [Ursus arctos] ID: MX.47348
#> [Moomin      ] Not found
taxa[[1]]
#> Ursus arctos 
#>   "MX.47348"
taxa[[2]]
#> Moomin 
#>     NA
```

You can also specify the taxonomic rank when searching FinBIF and the search
will be limited to the specified rank.

```r
finbif_check_taxa(list(species = c("Ursus arctos", "Ursus"), genus = "Ursus"))
#> [species: Ursus arctos] ID: MX.47348
#> [species: Ursus       ] Not found
#> [genus:   Ursus       ] ID: MX.51311
```

The function `finbif_taxa()` can be used for a more general search for taxa in 
the FinBIF database. Searches can be `exact`, `partial` or `likely` (fuzzy
matching). Information for a single taxon is returned when using exact or fuzzy
matching, but multiple taxa, up to a limit, `n`, may be returned when using
partial matching.

```r
birch_search <- finbif_taxa("Betula pendula", 2, "partial")
str(birch_search$content, max.level = 2, list.len = 8)
#> List of 2
#>  $ :List of 11
#>   ..$ matchingName  : chr "Betula pendula var. pendula"
#>   ..$ nameType      : chr "MX.scientificName"
#>   ..$ id            : chr "MX.37994"
#>   ..$ scientificName: chr "Betula pendula var. pendula"
#>   ..$ taxonRank     : chr "MX.variety"
#>   ..$ cursiveName   : logi TRUE
#>   ..$ finnish       : logi TRUE
#>   ..$ species       : logi TRUE
#>   .. [list output truncated]
#>  $ :List of 12
#>   ..$ matchingName            : chr "Betula pendula var. carelica"
#>   ..$ nameType                : chr "MX.scientificName"
#>   ..$ id                      : chr "MX.37997"
#>   ..$ scientificName          : chr "Betula pendula var. carelica"
#>   ..$ scientificNameAuthorship: chr "(Merckl.) Hämet-Ahti"
#>   ..$ taxonRank               : chr "MX.variety"
#>   ..$ cursiveName             : logi TRUE
#>   ..$ finnish                 : logi TRUE
#>   .. [list output truncated]
```

## Getting occurrence data
You can download occurrence data from the FinBIF database as a `data.frame` with
the `finbif_occurrence()` function.

```r
finbif_occurrence("Cygnus cygnus", n = 100)
#> Records downloaded: 100
#> Records available: 54828
#> A data.frame [100 x 30]
#>    scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1    Cygnus cygnus         1  60.50999  23.70616 2019-08-17 21:00:00
#> 2    Cygnus cygnus         1  60.56745  21.57191 2019-06-29 21:00:00
#> 3    Cygnus cygnus         1  61.32291  28.56818 2019-08-10 06:23:00
#> 4    Cygnus cygnus         1  62.56744  26.35088 2019-08-06 21:00:00
#> 5    Cygnus cygnus         1  61.32290  28.56811 2019-05-09 08:30:00
#> 6    Cygnus cygnus         1  61.32294  28.56868 2019-05-10 04:45:00
#> 7    Cygnus cygnus         1  61.07692  21.49222 2019-05-16 08:25:00
#> 8    Cygnus cygnus         1  62.25243  25.70933 2019-05-15 21:00:00
#> 9    Cygnus cygnus         1  60.83907  21.25772 2019-05-16 20:00:00
#> 10   Cygnus cygnus         1  61.12486  21.54164 2019-05-21 06:30:00
#> ...with 90 more records and 25 more variables:
#> taxon_rank, country, province, municipality, date_start, date_end,
#> hour_start, hour_end, minute_start, minute_end, record_id,
#> individual_id, event_id, collection_id, any_issues, record_issue,
#> record_reliable, taxon_reliability, document_issue,
#> document_reliablity, coordinate_accuracy, event_issue,
#> location_issue, time_issue, duration
```

You can search for multiple taxa at once and filter the records with the
`filter` argument.

```r
finbif_occurrence(
  "Cygnus cygnus", 
  "Cygnus olor",
  filter = list(coordinate_accuracy_max = 100)
)
#> Records downloaded: 10
#> Records available: 11175
#> A data.frame [10 x 30]
#>    scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1    Cygnus cygnus         1  60.50999  23.70616 2019-08-17 21:00:00
#> 2      Cygnus olor         2  60.42794  22.20052 2019-08-03 21:00:00
#> 3      Cygnus olor         2  60.42794  22.20052 2019-08-03 21:00:00
#> 4    Cygnus cygnus         2  60.83577  21.25200 2019-08-06 21:00:00
#> 5      Cygnus olor         2  60.42794  22.20052 2019-08-12 21:00:00
#> 6      Cygnus olor         2  60.42794  22.20052 2019-08-12 21:00:00
#> 7    Cygnus cygnus         2  60.91596  22.09577 2019-08-17 10:30:00
#> 8    Cygnus cygnus         3  60.83577  21.25200 2019-08-17 21:00:00
#> 9      Cygnus olor         4  60.56783  21.57229 2019-07-18 21:00:00
#> 10     Cygnus olor        10  60.56783  21.57229 2019-07-18 21:00:00
#> ...with 0 more records and 25 more variables:
#> taxon_rank, country, province, municipality, date_start, date_end,
#> hour_start, hour_end, minute_start, minute_end, record_id,
#> individual_id, event_id, collection_id, any_issues, record_issue,
#> record_reliable, taxon_reliability, document_issue,
#> document_reliablity, coordinate_accuracy, event_issue,
#> location_issue, time_issue, duration
```

See `?filters` and `vignette("filtering")` for more details on filtering FinBIF
records.

## Caching
By default `finbif` uses local filesystem caching for repeated API request. This
can be turned on or off on a per request or session basis. See `?caching` for
details.
