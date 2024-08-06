---
title: "Introduction to the finbif package"
author: "William K. Morris"
output: 
  rmarkdown::html_vignette:
    toc: true
vignette: >
  %\VignetteIndexEntry{1. Introduction to the finbif package}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---


FinBIF aggregates Finnish biodiversity data from multiple sources in a single
open access portal for researchers, citizen scientists, industry and government.
FinBIF allows users of biodiversity information to find, access, combine and 
visualise data on Finnish plants, animals and microorganisms. The `{finbif}`
R package makes the publicly available data in FinBIF easily accessible to
programmers. Biodiversity information is available on taxonomy and taxon
occurrence. Occurrence data can be filtered by taxon, time, location and other
variables. The data accessed are conveniently preformatted for subsequent
analyses.

## Installing the finbif package
You can install the current stable version of `{finbif}` from
[CRAN](https://cran.r-project.org),

``` r
install.packages("finbif")
```

You can also install the latest development version of `{finbif}` from
[GitHub](https://github.com),

``` r
remotes::install_github("luomus/finbif@dev")
```

## Loading the finbif package

``` r
library(finbif)
```

## Getting a FinBIF access token
To use the FinBIF API you must first request and set a personal access token.
You can request an API token to be sent to your email address with the function
`finbif_get_token()`.

``` r
finbif_request_token("your@email.com")
```

Copy the access token that was sent to your email and set it as the environment
variable `FINBIF_ACCESS_TOKEN` either for the current session,

``` r
Sys.setenv(
  FINBIF_ACCESS_TOKEN = "xtmSOIxjPwq0pOMB1WvcZgFLU9QBklauOlonWl8K5oaLIx8RniJLrv"
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

If the taxon is in the FinBIF database its unique ID is returned. When a taxon
is not in the FinBIF database it is reported as "not found" and for that taxa
the list element is `NA`.

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
birch_search$content
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> [[1]]
#> [[1]]$matchingName
#> [1] "Betula pendula var. pendula"
#> 
#> [[1]]$nameType
#> [1] "MX.scientificName"
#> 
#> [[1]]$id
#> [1] "MX.37994"
#> 
#> [[1]]$scientificName
#> [1] "Betula pendula var. pendula"
#> 
#> [[1]]$taxonRank
#> [1] "MX.variety"
#> 
#> [[1]]$cursiveName
#> [1] TRUE
#> 
#> [[1]]$finnish
#> [1] TRUE
#> 
#> [[1]]$species
#> [1] TRUE
#> 
#> [[1]]$vernacularName
#> [[1]]$vernacularName$sv
#> [1] "vanlig vårtbjörk"
#> 
#> [[1]]$vernacularName$fi
#> [1] "vihtakoivu"
#> 
#> 
#> [[1]]$informalGroups
#> [[1]]$informalGroups[[1]]
#> [[1]]$informalGroups[[1]]$id
#> [1] "MVL.343"
#> 
#> [[1]]$informalGroups[[1]]$name
#> [[1]]$informalGroups[[1]]$name$fi
#> [1] "Putkilokasvit"
#> 
#> [[1]]$informalGroups[[1]]$name$en
#> [1] "Vascular plants"
#> 
#> [[1]]$informalGroups[[1]]$name$sv
#> [1] "Kärlväxter"
#> 
#> 
#> 
#> 
#> [[1]]$kingdomScientificName
#> [1] "Plantae"
#> 
#> [[1]]$type
#> [1] "partialMatches"
#> 
#> 
#> [[2]]
#> [[2]]$matchingName
#> [1] "Betula nana × pendula"
#> 
#> [[2]]$nameType
#> [1] "MX.scientificName"
#> 
#> [[2]]$id
#> [1] "MX.38005"
#> 
#> [[2]]$scientificName
#> [1] "Betula nana × pendula"
#> 
#> [[2]]$taxonRank
#> [1] "MX.infragenericHybrid"
#> 
#> [[2]]$cursiveName
#> [1] TRUE
#> 
#> [[2]]$finnish
#> [1] TRUE
#> 
#> [[2]]$species
#> [1] TRUE
#> 
#> [[2]]$vernacularName
#> named list()
#> 
#> [[2]]$informalGroups
#> [[2]]$informalGroups[[1]]
#> [[2]]$informalGroups[[1]]$id
#> [1] "MVL.343"
#> 
#> [[2]]$informalGroups[[1]]$name
#> [[2]]$informalGroups[[1]]$name$fi
#> [1] "Putkilokasvit"
#> 
#> [[2]]$informalGroups[[1]]$name$en
#> [1] "Vascular plants"
#> 
#> [[2]]$informalGroups[[1]]$name$sv
#> [1] "Kärlväxter"
#> 
#> 
#> 
#> 
#> [[2]]$kingdomScientificName
#> [1] "Plantae"
#> 
#> [[2]]$type
#> [1] "partialMatches"
#> 
#> 

```

</details>
<br>

## Getting occurrence data
You can download occurrence data from the FinBIF database as a `data.frame` with
the `finbif_occurrence()` function.

```r
finbif_occurrence("Cygnus cygnus", n = 100)
#> Records downloaded: 100
#> Records available: 95730
#> A data.frame [100 x 12]
#>                   record_id      scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1            …JX.1594164#13 Cygnus cygnus (Linn…        NA  64.94473  26.67958 2023-06-13 14:33:00
#> 2  …HR.4412/6489172c9ddda_U Cygnus cygnus (Linn…        NA  61.74701  23.11493 2023-06-13 12:00:00
#> 3  …HR.4412/64891730060a4_U Cygnus cygnus (Linn…        NA  61.38348  22.97288 2023-06-13 12:00:00
#> 4  …HR.4412/648917378a3b6_U Cygnus cygnus (Linn…        NA  62.76028  24.15774 2023-06-13 12:00:00
#> 5  …HR.4412/6489175adc05f_U Cygnus cygnus (Linn…        NA  60.78752  21.39263 2023-06-13 12:00:00
#> 6  …HR.4412/6489173a1db9b_U Cygnus cygnus (Linn…        NA  64.31374  26.68643 2023-06-13 12:00:00
#> 7  …HR.4412/648917456a396_U Cygnus cygnus (Linn…        NA  61.87986  25.19067 2023-06-13 12:00:00
#> 8  …HR.4412/648917454912b_U Cygnus cygnus (Linn…        NA  60.42215  24.00099 2023-06-13 12:00:00
#> 9  …HR.4412/64891750e74e7_U Cygnus cygnus (Linn…        NA  63.86383  27.70835 2023-06-13 12:00:00
#> 10 …HR.4412/64891741330f8_U Cygnus cygnus (Linn…        NA  61.74701  23.11493 2023-06-13 12:00:00
#> ...with 90 more records and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality
```

You can search for multiple taxa at once and filter the records with the
`filter` argument.

```r
finbif_occurrence(
  "Cygnus cygnus",
  "Cygnus olor",
  filter = list(coordinates_uncertainty_max = 100)
)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 47293
#> A data.frame [10 x 12]
#>               record_id      scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1  …HR.3211/167194019-U Cygnus olor (J.F. G…        NA  60.12778  24.69528 2023-06-13 12:00:00
#> 2        …JX.1593978#37 Cygnus olor (J.F. G…  10        60.428    22.20038 2023-06-13 12:00:00
#> 3        …JX.1594262#19 Cygnus olor (J.F. G…  5         61.60789  21.51679 2023-06-13 12:00:00
#> 4        …JX.1594262#17 Cygnus cygnus (Linn…  5         61.60789  21.51679 2023-06-13 12:00:00
#> 5       …JX.1594053#296 Cygnus cygnus (Linn…  1         63.62644  23.70806 2023-06-13 03:10:00
#> 6        …JX.1594054#93 Cygnus cygnus (Linn…  1         63.63403  24.21195 2023-06-13 03:08:00
#> 7       …JX.1593931#255 Cygnus cygnus (Linn…  2         64.5582   27.78468 2023-06-13 05:03:00
#> 8         …JX.1593916#3 Cygnus cygnus (Linn…  2         66.11897  25.93173 2023-06-13 12:00:00
#> 9        …JX.1593873#63 Cygnus cygnus (Linn…  2         65.72167  27.94208 2023-06-13 09:27:00
#> 10 …HR.3211/167071964-U Cygnus olor (J.F. G…        NA  60.18156  24.93153 2023-06-12 12:00:00
#> ...with 0 more record and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

See `?filters` and `vignette("v05_filtering")` for more details on filtering
FinBIF records.

### Random sampling
It is possible to request a random sample of records instead of the last `n`
records (or records ordered by some other variable).

```r
finbif_occurrence("Birds", sample = TRUE)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 21748016
#> A data.frame [10 x 12]
#>                      record_id      scientific_name abundance lat_wgs84 lon_wgs84
#> 1  …HR.3691/OBS1143679721_Unit Aythya marila (Linn…  11        59.9815   24.39929
#> 2          …KE.67/1662813#Unit Fringilla coelebs L…  1         59.83333  19.93333
#> 3             …KE.8_1165614#90 Poecile montanus (C…  6         62.44825  29.92951
#> 4         …KE.67/12308933#Unit Prunella modularis …  1         61.67291  22.25931
#> 5          …KE.67/9586054#Unit Larus argentatus Po…  1         50.85     6.833333
#> 6          …KE.67/8576210#Unit Periparus ater (Lin…  1         60.2      19.33333
#> 7          …KE.67/9862495#Unit Prunella modularis …  1         61.13488  24.39512
#> 8          …KE.67/4669401#Unit Larus argentatus Po…  1         60.01667  24.61667
#> 9               …JX.1031300#61 Motacilla alba Linn…        NA  62.12915  30.16957
#> 10        …KE.67/11492752#Unit Bombycilla garrulus…  1         62.67061  30.93362
#> ...with 0 more record and 7 more variables:
#> date_time, coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

## Caching
By default `{finbif}` uses local caching for repeated API requests. This can be
turned on or off on a per request or session basis. See `?caching` for details.
