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
#> Records available: 55642
#> A data.frame [100 x 30]
#>    scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1    Cygnus cygnus         1  61.07692  21.49222 2019-10-09 05:50:00
#> 2    Cygnus cygnus         3  62.30263  24.52289 2019-10-09 04:45:00
#> 3    Cygnus cygnus         2  60.95615  21.68894 2019-10-08 06:07:00
#> 4    Cygnus cygnus         2  60.98465  21.70309 2019-10-08 05:59:00
#> 5    Cygnus cygnus         9  62.22049  24.59103 2019-10-08 04:54:00
#> 6    Cygnus cygnus         5  60.95401  26.09615 2019-09-29 21:00:00
#> 7    Cygnus cygnus         1  60.45848  22.37712 2019-09-28 21:00:00
#> 8    Cygnus cygnus         3  61.32291  28.56818 2019-09-29 00:00:00
#> 9    Cygnus cygnus         2  60.56745  21.57187 2019-09-28 21:00:00
#> 10   Cygnus cygnus         3  61.32291  28.56818 2019-09-27 07:46:00
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
#> Records available: 11273
#> A data.frame [10 x 30]
#>    scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1      Cygnus olor         6  61.07692  21.49222 2019-10-09 05:50:00
#> 2    Cygnus cygnus         1  61.07692  21.49222 2019-10-09 05:50:00
#> 3    Cygnus cygnus         2  60.95615  21.68894 2019-10-08 06:07:00
#> 4    Cygnus cygnus         2  60.98465  21.70309 2019-10-08 05:59:00
#> 5      Cygnus olor         2  60.42794  22.20052 2019-09-30 21:00:00
#> 6      Cygnus olor         4  60.42794  22.20052 2019-09-30 21:00:00
#> 7    Cygnus cygnus         5  60.95401  26.09615 2019-09-29 21:00:00
#> 8    Cygnus cygnus         1  60.45848  22.37712 2019-09-28 21:00:00
#> 9      Cygnus olor         5  60.56745  21.57187 2019-09-28 21:00:00
#> 10   Cygnus cygnus         2  60.56745  21.57187 2019-09-28 21:00:00
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

## Plotting occurrence data
The `finbif` package has a number of inbuilt functions for plotting (see e.g.,
`breaks_xy()` and `hist_xy()`). There is also an inbuilt dataset that can be
used to plot the border of Finland (`?finland_map`). Together these utilities
can be used to plot occurrences after they have been downloaded from FinBIF. For
example, the following can be used to plot the density of Eurasian Jay
occurrences from Finland.

```r
# Download all the occurrences of Eurasian Jay in Finland
# that have coordinates accurate to at least 100m
jays <- finbif_occurrence(
  taxa   = "Eurasian Jay",
  filter = c(
    coordinate_accuracy_max = 100,
    country                 = "Finland"
  ),
  n      = 2e4,
  quiet  = TRUE
)

# Compute the density of occurrences in 1/4 degree cells and plot as a heatmap
with(
  data = c(jays, finland_map),
  expr = {
    par(mar = c(5, 5, 1, 1), las = 1)
    # compute a 2d histogram from the occurrences
    breaks  <- breaks_xy(bbox, .25) # breakpoints every 1/4 of a degree
    density <- hist_xy(xy = list(lon_wgs84, lat_wgs84), breaks)
    # plot the histogram as a heatmap
    image(density,
          asp    = 2.4,
          breaks = 2^seq(0, 12), # breakpoints for the gridcell colours
          col    = hcl.colors(12, rev = TRUE),
          xlab   = "Longitude",
          ylab   = "Latitude",
          panel.first = grid())
    legend("topright",
           inset  = c(0, .01),
           legend = expression(2^12, "", "", 2^6, "", "", 2^0),
           fill   = hcl.colors(7),
           border = NA,
           bty    = "n",
           adj    = c(0, 0.25), 
           x.intersp = .2,
           y.intersp = .5)
    # add the Finnish border
    polygon(x = vertices, lwd = .2)
  }
)
```

![](plot-occurrences-1.png)

## Caching
By default `finbif` uses local filesystem caching for repeated API request. This
can be turned on or off on a per request or session basis. See `?caching` for
details.
