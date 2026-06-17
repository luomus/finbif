---
title: "Occurrence records from FinBIF"
author: "William K. Morris"
output: 
  rmarkdown::html_vignette:
    toc: true
vignette: >
  %\VignetteIndexEntry{2. Occurrence records from FinBIF}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



The core purpose of `{finbif}` is accessing occurrence data stored in the FinBIF
database. Occurrence data can be retrieved from FinBIF with the function
`finbif_occurrence()`. Without any arguments specified `finbif_occurrence()`
will retrieve the latest 10 occurrence records from FinBIF.

``` r
finbif_occurrence()
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 60179988
#> A data.frame [10 x 12]
#>    occurrenceID       scientificName individualCount decimalLatitude decimalLongitude
#> 1           …21 Polytrichum juniper…              NA  60.17967        24.914629      
#> 2           …25 Polytrichum juniper…              NA  60.373472       24.993816      
#> 3           …29 Polytrichum juniper…              NA  61.612783       21.44191       
#> 4           …33 Polytrichum juniper…              NA  61.322069       23.513515      
#> 5           …37 Polytrichum juniper…              NA  61.249458       25.040691      
#> 6           …41 Polytrichum juniper…              NA  62.605448       25.925676      
#> 7           …45 Polytrichum juniper…              NA  62.22789        30.629365      
#> 8           …49 Polytrichum juniper…              NA  66.004079       28.202282      
#> 9           …53 Polytrichum juniper…              NA  69.049179       20.812003      
#> 10          …57 Polytrichum pilifer…              NA  60.373472       24.993816      
#> ...with 0 more records and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus

```

</details>
<br>
The print method for the resulting `finbif_occ` object will display the number
of records downloaded, the total number of records available, a data summary
including up to 10 rows of some core record variables (when available), the
number of remaining records and variables, as well as the names of additional
variables.

## Darwin Core Variables
You can switch from the default [Darwin Core
](http://rs.tdwg.org/dwc/) style variable names by setting `dwc = FALSE`.

``` r
colnames(finbif_occurrence(dwc = FALSE))
#>  [1] "record_id"               "scientific_name"         "abundance"              
#>  [4] "lat_wgs84"               "lon_wgs84"               "date_time"              
#>  [7] "coordinates_uncertainty" "any_issues"              "requires_verification"  
#> [10] "requires_identification" "record_reliability"      "record_quality"
```
The functions `to_dwc()` and `to_native()` can be used to translate variable
names to and from Darwin Core style and `{finbif}`'s native variable names
style.

## Choosing taxa
You can limit the records to certain taxa by specifying them as an argument.

``` r
finbif_occurrence("Cygnus cygnus")
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 139837
#> A data.frame [10 x 12]
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
#> ...with 0 more records and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus

```

</details>
<br>

Multiple taxa can be requested at once.

``` r
finbif_occurrence("Cygnus cygnus", "Cygnus olor")
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 203083
#> A data.frame [10 x 12]
#>    occurrenceID       scientificName individualCount decimalLatitude decimalLongitude
#> 1      …0745928 Cygnus cygnus (Linn…              NA  66              29.25          
#> 2      …0745932 Cygnus cygnus (Linn…              NA              NA               NA
#> 3      …0745937 Cygnus cygnus (Linn…              NA              NA               NA
#> 4      …0745941 Cygnus cygnus (Linn…              NA  60.17           25             
#> 5      …0745945 Cygnus cygnus (Linn…              NA  60.17           25             
#> 6      …0745949 Cygnus cygnus (Linn…              NA  60.17           25             
#> 7      …0745953 Cygnus olor (J.F. G…              NA  60.1741         24.9439        
#> 8      …0745957 Cygnus olor (J.F. G…              NA              NA               NA
#> 9      …0745961 Cygnus olor (J.F. G…              NA              NA               NA
#> 10     …5552466 Cygnus cygnus (Linn…  1               67.773235       25.050461      
#> ...with 0 more records and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus

```

</details>
<br>

You can also chose higher taxonomic groups and use common names (in English,
Finnish and Swedish).

``` r
birds  <- finbif_occurrence("Birds")
linnut <- finbif_occurrence("Linnut")
faglar <- finbif_occurrence("Fåglar")

lapply(list(birds, linnut, faglar), nrow)
#> [[1]]
#> [1] 10
#> 
#> [[2]]
#> [1] 10
#> 
#> [[3]]
#> [1] 10
```

## Request size
You can increase the number of records returned by using the `n` argument.

``` r
finbif_occurrence(n = 1001)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 1001
#> Records available: 60182673
#> A data.frame [1001 x 12]
#>    occurrenceID       scientificName individualCount decimalLatitude decimalLongitude
#> 1           …21 Polytrichum juniper…              NA  60.17967        24.914629      
#> 2           …25 Polytrichum juniper…              NA  60.373472       24.993816      
#> 3           …29 Polytrichum juniper…              NA  61.612783       21.44191       
#> 4           …33 Polytrichum juniper…              NA  61.322069       23.513515      
#> 5           …37 Polytrichum juniper…              NA  61.249458       25.040691      
#> 6           …41 Polytrichum juniper…              NA  62.605448       25.925676      
#> 7           …45 Polytrichum juniper…              NA  62.22789        30.629365      
#> 8           …49 Polytrichum juniper…              NA  66.004079       28.202282      
#> 9           …53 Polytrichum juniper…              NA  69.049179       20.812003      
#> 10          …57 Polytrichum pilifer…              NA  60.373472       24.993816      
#> ...with 991 more records and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus

```

</details>
<br>

You can see how many records are available for a given request, without
retrieving any records, by setting `count_only = TRUE`.

``` r
finbif_occurrence(count_only = TRUE)
#> [1] 60182646
```

## Checking taxa
When you request occurrence records for specific taxa, by default, the taxon
names are first checked against the FinBIF database. If any of the requested
taxa are not found in the database you will receive a warning but the data will
still be retrieved for the remaining taxa.

``` r
finbif_occurrence("Vulpes vulpes", "Moomin")
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 8107
#> A data.frame [10 x 12]
#>    occurrenceID       scientificName individualCount decimalLatitude decimalLongitude
#> 1       …223360 Vulpes vulpes var. …              NA  56              160            
#> 2       …223410 Vulpes vulpes (Linn…              NA  42.87           76.72          
#> 3       …227292 Vulpes vulpes subsp…              NA              NA               NA
#> 4       …227296 Vulpes vulpes (Linn…              NA  58.7            13.83          
#> 5       …579385 Vulpes vulpes (Linn…              NA  61.13           25.77          
#> 6       …580318 Vulpes vulpes (Linn…              NA  59.931          23.706         
#> 7       …580322 Vulpes vulpes (Linn…              NA              NA               NA
#> 8       …580326 Vulpes vulpes (Linn…              NA              NA               NA
#> 9       …580330 Vulpes vulpes (Linn…              NA              NA               NA
#> 10      …580334 Vulpes vulpes (Linn…              NA  69              27.5           
#> ...with 0 more records and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus

```

</details>
<br>

You can turn off taxon name pre-checking by setting the value of the
`check_taxa` argument to `FALSE`.

``` r
finbif_occurrence("Vulpes vulpes", "Moomin", check_taxa = FALSE)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 8107
#> A data.frame [10 x 12]
#>    occurrenceID       scientificName individualCount decimalLatitude decimalLongitude
#> 1       …223360 Vulpes vulpes var. …              NA  56              160            
#> 2       …223410 Vulpes vulpes (Linn…              NA  42.87           76.72          
#> 3       …227292 Vulpes vulpes subsp…              NA              NA               NA
#> 4       …227296 Vulpes vulpes (Linn…              NA  58.7            13.83          
#> 5       …579385 Vulpes vulpes (Linn…              NA  61.13           25.77          
#> 6       …580318 Vulpes vulpes (Linn…              NA  59.931          23.706         
#> 7       …580322 Vulpes vulpes (Linn…              NA              NA               NA
#> 8       …580326 Vulpes vulpes (Linn…              NA              NA               NA
#> 9       …580330 Vulpes vulpes (Linn…              NA              NA               NA
#> 10      …580334 Vulpes vulpes (Linn…              NA  69              27.5           
#> ...with 0 more records and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus

```

</details>
<br>

By setting the argument, `on_check_fail` to `"error"` (the default is `"warn"`),
you can elevate the warnings to errors and the request will fail if any of the
taxa are not found in the FinBIF database.

``` r
finbif_occurrence("Vulpes vulpes", "Moomin", on_check_fail = "error")
#> Error:
#> ! Cannot find the following taxa in the FinBIF taxonomy.
#> Please check you are using accepted names and not synonyms or
#> other names for the taxa you are selecting:
#> 
#> Moomin
```
This can be a useful strategy if you are using `{finbif}` non-interactively
(in a script), and you do not want to proceed if any of your taxon names are
wrong or misspelled.

## Aggregating records
You can request records in aggregate using the `aggregate` argument to
`finbif_occurrence`. Aggregated requests will return counts for the combination
of the `variables` you specify with the `select` argument. You can request
counts of `"records"`, `"species"` or `"taxa"` by using the corresponding string
as the value for the `aggregate` argument. Aggregating by `"species"` will count
the number of unique species identifiers for a set of records grouped by the
combination of selected variables. Note that this count will not include records
of taxa that do not have species identifiers, including records of higher taxa
(e.g., genus only records), records of the non-species children of aggregate or
complex taxa, and hybrid taxa. Therefore, in some contexts the results returned
will be an underestimate of species richness. Likewise, aggregating by `"taxa"`,
which returns a count the number of unique taxon identifiers, could represent an
overestimate of the number of taxa as records of higher taxa will contribute to
the count while their true identify may be a duplicate of other records.

To illustrate, you can count the number of moths and butterflies by municipality
with the following:

``` r
finbif_occurrence(
  "Lepidoptera", select = "finnishCounty", aggregate = "species"
)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 309
#> A data.frame [10 x 2]
#>    finnishCounty n_species
#> 1      Raasepori  2078    
#> 2      Virolahti  2037    
#> 3       Rääkkylä  1396    
#> 4        Kouvola  1578    
#> 5    Kemiönsaari  2042    
#> 6          Hanko  1994    
#> 7       Parainen  1922    
#> 8       Helsinki  2069    
#> 9         Kuopio  1445    
#> 10         Turku  1827    

```

</details>
<br>

## Time & duration
The default behaviour of `finbif_occurrence` is to consolidate date and time
data for occurrence recording events into a `date_time` variable. This can be
turned off (which can speed up data processing time) by deselecting the
`date_time` variable.

``` r
finbif_occurrence(select = "-eventDateTime")
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 60179991
#> A data.frame [10 x 11]
#>    occurrenceID       scientificName individualCount decimalLatitude decimalLongitude
#> 1           …21 Polytrichum juniper…              NA  60.17967        24.914629      
#> 2           …25 Polytrichum juniper…              NA  60.373472       24.993816      
#> 3           …29 Polytrichum juniper…              NA  61.612783       21.44191       
#> 4           …33 Polytrichum juniper…              NA  61.322069       23.513515      
#> 5           …37 Polytrichum juniper…              NA  61.249458       25.040691      
#> 6           …41 Polytrichum juniper…              NA  62.605448       25.925676      
#> 7           …45 Polytrichum juniper…              NA  62.22789        30.629365      
#> 8           …49 Polytrichum juniper…              NA  66.004079       28.202282      
#> 9           …53 Polytrichum juniper…              NA  69.049179       20.812003      
#> 10          …57 Polytrichum pilifer…              NA  60.373472       24.993816      
#> ...with 0 more records and 6 more variables:
#> coordinateUncertaintyInMeters, hasIssues, requiresVerification, requiresIdentification,
#> occurrenceReliability, identificationVerificationStatus

```

</details>
<br>

### Timezone
#### Timezone input
The FinBIF database doesn't currently store timezone information, so `{finbif}`
makes assumptions about the appropriate timezone based on the time and location
of the occurrence recording events to calculate `date_time` and `duration`. By
default, a fast heuristic is used to determine the timezones. If you require
greater accuracy (e.g., you are using data on the Finnish/Swedish border and
daytime/nighttime hours are important), you can switch to more accurate, though
slower, timezone calculation method.

``` r
finbif_occurrence(date_time_method = "accurate")
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 60179988
#> A data.frame [10 x 12]
#>    occurrenceID       scientificName individualCount decimalLatitude decimalLongitude
#> 1           …21 Polytrichum juniper…              NA  60.17967        24.914629      
#> 2           …25 Polytrichum juniper…              NA  60.373472       24.993816      
#> 3           …29 Polytrichum juniper…              NA  61.612783       21.44191       
#> 4           …33 Polytrichum juniper…              NA  61.322069       23.513515      
#> 5           …37 Polytrichum juniper…              NA  61.249458       25.040691      
#> 6           …41 Polytrichum juniper…              NA  62.605448       25.925676      
#> 7           …45 Polytrichum juniper…              NA  62.22789        30.629365      
#> 8           …49 Polytrichum juniper…              NA  66.004079       28.202282      
#> 9           …53 Polytrichum juniper…              NA  69.049179       20.812003      
#> 10          …57 Polytrichum pilifer…              NA  60.373472       24.993816      
#> ...with 0 more records and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus

```

</details>
<br>

#### Timezone output
The timezone of the calculated `date_time` variable is determined by the
timezone of your operating system.

``` r
Sys.timezone()
```

You can override this by setting the `tzone` argument to a different value.

``` r
finbif_occurrence(tzone = "Etc/UTC")
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 60179988
#> A data.frame [10 x 12]
#>    occurrenceID       scientificName individualCount decimalLatitude decimalLongitude
#> 1           …21 Polytrichum juniper…              NA  60.17967        24.914629      
#> 2           …25 Polytrichum juniper…              NA  60.373472       24.993816      
#> 3           …29 Polytrichum juniper…              NA  61.612783       21.44191       
#> 4           …33 Polytrichum juniper…              NA  61.322069       23.513515      
#> 5           …37 Polytrichum juniper…              NA  61.249458       25.040691      
#> 6           …41 Polytrichum juniper…              NA  62.605448       25.925676      
#> 7           …45 Polytrichum juniper…              NA  62.22789        30.629365      
#> 8           …49 Polytrichum juniper…              NA  66.004079       28.202282      
#> 9           …53 Polytrichum juniper…              NA  69.049179       20.812003      
#> 10          …57 Polytrichum pilifer…              NA  60.373472       24.993816      
#> ...with 0 more records and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus

```

</details>
<br>

Or set the global timezone option to set the timezone for the current session.

``` r
options(finbif_tz = "Etc/UTC")
```
This may be advisable for reproducibility or when working with multiple systems.
