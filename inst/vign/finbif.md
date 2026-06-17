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

``` r
finbif_check_taxa("Ursus arctos")
#> [Ursus arctos] ID: MX.47348
```

If the taxon is in the FinBIF database its unique ID is returned. When a taxon
is not in the FinBIF database it is reported as "not found" and for that taxa
the list element is `NA`.

``` r
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

``` r
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

``` r
birch_search <- finbif_taxa("Betula pendula", 2, "partial")
birch_search$content
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> $results
#> $results[[1]]
#> $results[[1]]$matchingName
#> [1] "Betula pendula var. pendula"
#> 
#> $results[[1]]$nameType
#> [1] "MX.scientificName"
#> 
#> $results[[1]]$id
#> [1] "MX.37994"
#> 
#> $results[[1]]$checklist
#> [1] "MR.1"
#> 
#> $results[[1]]$scientificName
#> [1] "Betula pendula var. pendula"
#> 
#> $results[[1]]$taxonRank
#> [1] "MX.variety"
#> 
#> $results[[1]]$cursiveName
#> [1] TRUE
#> 
#> $results[[1]]$finnish
#> [1] TRUE
#> 
#> $results[[1]]$species
#> [1] TRUE
#> 
#> $results[[1]]$vernacularName
#> [1] "vihtakoivu"
#> 
#> $results[[1]]$informalGroups
#> $results[[1]]$informalGroups[[1]]
#> $results[[1]]$informalGroups[[1]]$id
#> [1] "MVL.343"
#> 
#> $results[[1]]$informalGroups[[1]]$name
#> [1] "Vascular plants"
#> 
#> 
#> 
#> $results[[1]]$kingdomScientificName
#> [1] "Plantae"
#> 
#> $results[[1]]$type
#> [1] "partialMatches"
#> 
#> 
#> $results[[2]]
#> $results[[2]]$matchingName
#> [1] "Betula pendula × pubescens"
#> 
#> $results[[2]]$nameType
#> [1] "MX.scientificName"
#> 
#> $results[[2]]$id
#> [1] "MX.37998"
#> 
#> $results[[2]]$checklist
#> [1] "MR.1"
#> 
#> $results[[2]]$scientificName
#> [1] "Betula pendula × pubescens"
#> 
#> $results[[2]]$taxonRank
#> [1] "MX.infragenericHybrid"
#> 
#> $results[[2]]$cursiveName
#> [1] TRUE
#> 
#> $results[[2]]$finnish
#> [1] TRUE
#> 
#> $results[[2]]$species
#> [1] TRUE
#> 
#> $results[[2]]$vernacularName
#> [1] "hybridikoivu"
#> 
#> $results[[2]]$informalGroups
#> $results[[2]]$informalGroups[[1]]
#> $results[[2]]$informalGroups[[1]]$id
#> [1] "MVL.343"
#> 
#> $results[[2]]$informalGroups[[1]]$name
#> [1] "Vascular plants"
#> 
#> 
#> 
#> $results[[2]]$kingdomScientificName
#> [1] "Plantae"
#> 
#> $results[[2]]$type
#> [1] "partialMatches"
#> 
#> 
#> 
#> $`@context`
#> [1] "https://api.laji.fi/context/taxon-search-en"
#> 

```

</details>
<br>

## Getting occurrence data
You can download occurrence data from the FinBIF database as a `data.frame` with
the `finbif_occurrence()` function.

``` r
finbif_occurrence("Cygnus cygnus", n = 100)
#> Records downloaded: 100
#> Records available: 139837
#> A data.frame [100 x 12]
#>    occurrenceID       scientificName individualCount decimalLatitude decimalLongitude
#> 1      …0745928 Cygnus cygnus (Linn…              NA  66              29.25          
#> 2      …0745932 Cygnus cygnus (Linn…              NA              NA               NA
#> 3      …0745937 Cygnus cygnus (Linn…              NA              NA               NA
#> 4      …0745941 Cygnus cygnus (Linn…              NA  60.17           25             
#> 5      …0745945 Cygnus cygnus (Linn…              NA  60.17           25             
#> 6      …0745949 Cygnus cygnus (Linn…              NA  60.17           25             
#> 7      …5552466 Cygnus cygnus (Linn…  1               67.773235       25.050461      
#> 8      …5552470 Cygnus cygnus (Linn…  4               69.063377       27.11982       
#> 9      …5552474 Cygnus cygnus (Linn…  1               68.713901       22.787894      
#> 10     …5559118 Cygnus cygnus (Linn…  3                           NA               NA
#> ...with 90 more records and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus
```

You can search for multiple taxa at once and filter the records with the
`filter` argument.

``` r
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
#> Records available: 87466
#> A data.frame [10 x 12]
#>    occurrenceID       scientificName individualCount decimalLatitude decimalLongitude
#> 1        …33947 Cygnus cygnus (Linn…              NA  63.4            33.39          
#> 2        …33951 Cygnus cygnus (Linn…              NA  63.4            33.39          
#> 3        …35003 Cygnus cygnus (Linn…              NA  63.4            33.39          
#> 4        …39895 Cygnus cygnus (Linn…              NA  61.2            22.7           
#> 5        …40419 Cygnus olor (J.F. G…              NA  60.36           26.77          
#> 6        …41419 Cygnus cygnus (Linn…              NA  60.33           25.67          
#> 7        …41535 Cygnus cygnus (Linn…              NA  61.29           25.83          
#> 8        …43347 Cygnus cygnus (Linn…              NA  60.571967       27.489174      
#> 9        …43371 Cygnus olor (J.F. G…              NA  60.4517         23.9772        
#> 10       …43491 Cygnus cygnus (Linn…              NA  60.3249         25.7125        
#> ...with 0 more records and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus

```

</details>
<br>

See `?filters` and `vignette("v05_filtering")` for more details on filtering
FinBIF records.

### Random sampling
It is possible to request a random sample of records instead of the last `n`
records (or records ordered by some other variable).

``` r
finbif_occurrence("Birds", sample = TRUE)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 29078618
#> A data.frame [10 x 12]
#>            occurrenceID       scientificName individualCount decimalLatitude
#> 1        …JX.1026053#46 Anthus trivialis (L…  1               64.300242     
#> 2      …KE.8_1044172#41 Turdus merula Linna…  1               60.458446     
#> 3   …KE.67/2710655#Unit Rallus aquaticus Li…  1               60.197895     
#> 4    …KE.67/507788#Unit Parus major Linnaeu…  1               60.237614     
#> 5        …HR.49/41446_U Astur gentilis (Lin…  1               62.229788     
#> 6    …KE.67/353020#Unit Parus major Linnaeu…  1               60.266667     
#> 7  …KE.67/12431497#Unit Riparia riparia (Li…  1               61.072328     
#> 8          …JX.94072#20 Poecile montanus (C…  12              60.788433     
#> 9       …JX.1247883#256 Grus grus (Linnaeus…  1               64.562534     
#> 10 …KE.67/14598103#Unit Coloeus monedula (L…  1               62.782648     
#> ...with 0 more records and 8 more variables:
#> decimalLongitude, eventDateTime, coordinateUncertaintyInMeters, hasIssues,
#> requiresVerification, requiresIdentification, occurrenceReliability,
#> identificationVerificationStatus

```

</details>
<br>

## Caching
By default `{finbif}` uses local caching for repeated API requests. This can be
turned on or off on a per request or session basis. See `?caching` for details.
