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

```r
finbif_occurrence()
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 47159747
#> A data.frame [10 x 12]
#>                                 record_id      scientific_name abundance lat_wgs84 lon_wgs84
#> 1  …KE.176/64895825d5de884fa20e297d#Unit1 Heracleum persicum …        NA  61.08302  22.38983
#> 2                           …JX.1594382#9 Hirundo rustica Lin…        NA  64.12716  23.99111
#> 3                          …JX.1594382#37 Pica pica (Linnaeus…        NA  64.12716  23.99111
#> 4                          …JX.1594382#49 Muscicapa striata (…        NA  64.12716  23.99111
#> 5                          …JX.1594382#39 Larus canus Linnaeu…        NA  64.12716  23.99111
#> 6                           …JX.1594382#5 Emberiza citrinella…        NA  64.12716  23.99111
#> 7                          …JX.1594382#31 Ficedula hypoleuca …        NA  64.12716  23.99111
#> 8                          …JX.1594382#41 Alauda arvensis Lin…        NA  64.12716  23.99111
#> 9                          …JX.1594382#21 Numenius arquata (L…        NA  64.12716  23.99111
#> 10                         …JX.1594382#29 Dendrocopos major (…        NA  64.12716  23.99111
#> ...with 0 more record and 7 more variables:
#> date_time, coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>
The print method for the resulting `finbif_occ` object will display the number
of records downloaded, the total number of records available, a data summary
including up to 10 rows of some core record variables (when available), the
number of remaining records and variables, as well as the names of additional
variables.

## Darwin Core Variables
You can switch from the default variable names to [Darwin Core
](http://rs.tdwg.org/dwc/) style names by setting `dwc = TRUE`.

```r
colnames(finbif_occurrence(dwc = TRUE))
#>  [1] "occurrenceID"                  "scientificName"                "individualCount"              
#>  [4] "decimalLatitude"               "decimalLongitude"              "eventDateTime"                
#>  [7] "coordinateUncertaintyInMeters" "hasIssues"                     "requiresVerification"         
#> [10] "requiresIdentification"        "occurrenceReliability"         "occurrenceQuality"
```
The functions `to_dwc()` and `to_native()` can be used to translate variable
names to and from Darwin Core style and `{finbif}`'s native variable names
style.

## Choosing taxa
You can limit the records to certain taxa by specifying them as an argument.

```r
finbif_occurrence("Cygnus cygnus")
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 95730
#> A data.frame [10 x 12]
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
#> ...with 0 more record and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

Multiple taxa can be requested at once.

```r
finbif_occurrence("Cygnus cygnus", "Cygnus olor")
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 138681
#> A data.frame [10 x 12]
#>                   record_id      scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1            …JX.1594164#13 Cygnus cygnus (Linn…        NA  64.94473  26.67958 2023-06-13 14:33:00
#> 2  …HR.4412/6489172c9ddda_U Cygnus cygnus (Linn…        NA  61.74701  23.11493 2023-06-13 12:00:00
#> 3  …HR.4412/64891730060a4_U Cygnus cygnus (Linn…        NA  61.38348  22.97288 2023-06-13 12:00:00
#> 4  …HR.4412/648917378a3b6_U Cygnus cygnus (Linn…        NA  62.76028  24.15774 2023-06-13 12:00:00
#> 5  …HR.4412/64891759be4c7_U Cygnus olor (J.F. G…        NA  63.40045  21.48901 2023-06-13 12:00:00
#> 6  …HR.4412/6489175adc05f_U Cygnus cygnus (Linn…        NA  60.78752  21.39263 2023-06-13 12:00:00
#> 7  …HR.4412/6489173a1db9b_U Cygnus cygnus (Linn…        NA  64.31374  26.68643 2023-06-13 12:00:00
#> 8  …HR.4412/648917456a396_U Cygnus cygnus (Linn…        NA  61.87986  25.19067 2023-06-13 12:00:00
#> 9  …HR.4412/648917454912b_U Cygnus cygnus (Linn…        NA  60.42215  24.00099 2023-06-13 12:00:00
#> 10 …HR.4412/64891750e74e7_U Cygnus cygnus (Linn…        NA  63.86383  27.70835 2023-06-13 12:00:00
#> ...with 0 more record and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

You can also chose higher taxonomic groups and use common names (in English,
Finnish and Swedish).

```r
birds  <- finbif_occurrence("Birds")
linnut <- finbif_occurrence("Linnut")
faglar <- finbif_occurrence("Fåglar")

sapply(list(birds, linnut, faglar), nrow)
#> [1] 10 10 10
```

## Request size
You can increase the number of records returned by using the `n` argument.

```r
finbif_occurrence(n = 1001)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 1001
#> Records available: 47159747
#> A data.frame [1001 x 12]
#>                                 record_id      scientific_name abundance lat_wgs84 lon_wgs84
#> 1  …KE.176/64895825d5de884fa20e297d#Unit1 Heracleum persicum …        NA  61.08302  22.38983
#> 2                           …JX.1594382#9 Hirundo rustica Lin…        NA  64.12716  23.99111
#> 3                          …JX.1594382#37 Pica pica (Linnaeus…        NA  64.12716  23.99111
#> 4                          …JX.1594382#49 Muscicapa striata (…        NA  64.12716  23.99111
#> 5                          …JX.1594382#39 Larus canus Linnaeu…        NA  64.12716  23.99111
#> 6                           …JX.1594382#5 Emberiza citrinella…        NA  64.12716  23.99111
#> 7                          …JX.1594382#31 Ficedula hypoleuca …        NA  64.12716  23.99111
#> 8                          …JX.1594382#41 Alauda arvensis Lin…        NA  64.12716  23.99111
#> 9                          …JX.1594382#21 Numenius arquata (L…        NA  64.12716  23.99111
#> 10                         …JX.1594382#29 Dendrocopos major (…        NA  64.12716  23.99111
#> ...with 991 more records and 7 more variables:
#> date_time, coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

You can see how many records are available for a given request, without
retrieving any records, by setting `count_only = TRUE`.

```r
finbif_occurrence(count_only = TRUE)
#> [1] 47159747
```

## Checking taxa
When you request occurrence records for specific taxa, by default, the taxon
names are first checked against the FinBIF database. If any of the requested
taxa are not found in the database you will receive a warning but the data will
still be retrieved for the remaining taxa.

```r
finbif_occurrence("Vulpes vulpes", "Moomin")
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 5303
#> A data.frame [10 x 12]
#>                                 record_id      scientific_name abundance lat_wgs84 lon_wgs84
#> 1                    …HR.3211/167313561-U Vulpes vulpes (Linn…        NA  60.18049  25.04838
#> 2                    …HR.3211/167310567-U Vulpes vulpes (Linn…        NA  60.2241   24.89373
#> 3  …KE.176/64894ccdd5de884fa20e2972#Unit1 Vulpes vulpes (Linn…  1         60.21118  24.90744
#> 4  …KE.176/6489506dd5de884fa20e2976#Unit1 Vulpes vulpes (Linn…  1         60.11016  25.01864
#> 5  …KE.176/648802d6d5de884fa20e290d#Unit1 Vulpes vulpes (Linn…  1         60.11016  25.01864
#> 6  …KE.176/648802c7d5de884fa20e290c#Unit1 Vulpes vulpes (Linn…  1         60.11016  25.01864
#> 7                    …HR.3211/167167234-U Vulpes vulpes (Linn…        NA  60.20261  24.86879
#> 8                    …HR.3211/166968734-U Vulpes vulpes (Linn…        NA  60.5      21.9    
#> 9                    …HR.3211/166944731-U Vulpes vulpes (Linn…        NA  60.17493  24.74123
#> 10 …KE.176/64869a52d5de884fa20e28ae#Unit1 Vulpes vulpes (Linn…  1         60.23885  25.12012
#> ...with 0 more record and 7 more variables:
#> date_time, coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

You can turn off taxon name pre-checking by setting the value of the
`check_taxa` argument to `FALSE`.

```r
finbif_occurrence("Vulpes vulpes", "Moomin", check_taxa = FALSE)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 5303
#> A data.frame [10 x 12]
#>                                 record_id      scientific_name abundance lat_wgs84 lon_wgs84
#> 1                    …HR.3211/167313561-U Vulpes vulpes (Linn…        NA  60.18049  25.04838
#> 2                    …HR.3211/167310567-U Vulpes vulpes (Linn…        NA  60.2241   24.89373
#> 3  …KE.176/64894ccdd5de884fa20e2972#Unit1 Vulpes vulpes (Linn…  1         60.21118  24.90744
#> 4  …KE.176/6489506dd5de884fa20e2976#Unit1 Vulpes vulpes (Linn…  1         60.11016  25.01864
#> 5  …KE.176/648802d6d5de884fa20e290d#Unit1 Vulpes vulpes (Linn…  1         60.11016  25.01864
#> 6  …KE.176/648802c7d5de884fa20e290c#Unit1 Vulpes vulpes (Linn…  1         60.11016  25.01864
#> 7                    …HR.3211/167167234-U Vulpes vulpes (Linn…        NA  60.20261  24.86879
#> 8                    …HR.3211/166968734-U Vulpes vulpes (Linn…        NA  60.5      21.9    
#> 9                    …HR.3211/166944731-U Vulpes vulpes (Linn…        NA  60.17493  24.74123
#> 10 …KE.176/64869a52d5de884fa20e28ae#Unit1 Vulpes vulpes (Linn…  1         60.23885  25.12012
#> ...with 0 more record and 7 more variables:
#> date_time, coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

By setting the argument, `on_check_fail` to `"error"` (the default is `"warn"`),
you can elevate the warnings to errors and the request will fail if any of the
taxa are not found in the FinBIF database.

```r
finbif_occurrence("Vulpes vulpes", "Moomin", on_check_fail = "error")
#> Error: Cannot find the following taxa in the FinBIF taxonomy.
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

```r
finbif_occurrence("Lepidoptera", select = "municipality", aggregate = "species")
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 309
#> A data.frame [10 x 2]
#>    municipality n_species
#> 1     Raasepori  2038    
#> 2     Virolahti  1978    
#> 3       Kouvola  1508    
#> 4      Rääkkylä  1367    
#> 5   Kemiönsaari  2008    
#> 6         Hanko  1945    
#> 7      Parainen  1858    
#> 8      Helsinki  1980    
#> 9        Kuopio  1345    
#> 10        Kotka  1719    

```

</details>
<br>

## Time & duration
The default behaviour of `finbif_occurrence` is to consolidate date and time
data for occurrence recording events into a `date_time` variable. This can be
turned off (which can speed up data processing time) by deselecting the
`date_time` variable.

```r
finbif_occurrence(select = "-date_time")
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 47159747
#> A data.frame [10 x 11]
#>                                 record_id      scientific_name abundance lat_wgs84 lon_wgs84
#> 1  …KE.176/64895825d5de884fa20e297d#Unit1 Heracleum persicum …        NA  61.08302  22.38983
#> 2                           …JX.1594382#9 Hirundo rustica Lin…        NA  64.12716  23.99111
#> 3                          …JX.1594382#37 Pica pica (Linnaeus…        NA  64.12716  23.99111
#> 4                          …JX.1594382#49 Muscicapa striata (…        NA  64.12716  23.99111
#> 5                          …JX.1594382#39 Larus canus Linnaeu…        NA  64.12716  23.99111
#> 6                           …JX.1594382#5 Emberiza citrinella…        NA  64.12716  23.99111
#> 7                          …JX.1594382#31 Ficedula hypoleuca …        NA  64.12716  23.99111
#> 8                          …JX.1594382#41 Alauda arvensis Lin…        NA  64.12716  23.99111
#> 9                          …JX.1594382#21 Numenius arquata (L…        NA  64.12716  23.99111
#> 10                         …JX.1594382#29 Dendrocopos major (…        NA  64.12716  23.99111
#> ...with 0 more record and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

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

```r
finbif_occurrence(date_time_method = "accurate")
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 47159747
#> A data.frame [10 x 12]
#>                                 record_id      scientific_name abundance lat_wgs84 lon_wgs84
#> 1  …KE.176/64895825d5de884fa20e297d#Unit1 Heracleum persicum …        NA  61.08302  22.38983
#> 2                           …JX.1594382#9 Hirundo rustica Lin…        NA  64.12716  23.99111
#> 3                          …JX.1594382#37 Pica pica (Linnaeus…        NA  64.12716  23.99111
#> 4                          …JX.1594382#49 Muscicapa striata (…        NA  64.12716  23.99111
#> 5                          …JX.1594382#39 Larus canus Linnaeu…        NA  64.12716  23.99111
#> 6                           …JX.1594382#5 Emberiza citrinella…        NA  64.12716  23.99111
#> 7                          …JX.1594382#31 Ficedula hypoleuca …        NA  64.12716  23.99111
#> 8                          …JX.1594382#41 Alauda arvensis Lin…        NA  64.12716  23.99111
#> 9                          …JX.1594382#21 Numenius arquata (L…        NA  64.12716  23.99111
#> 10                         …JX.1594382#29 Dendrocopos major (…        NA  64.12716  23.99111
#> ...with 0 more record and 7 more variables:
#> date_time, coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

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

```r
finbif_occurrence(tzone = "Etc/UTC")
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 47159747
#> A data.frame [10 x 12]
#>                                 record_id      scientific_name abundance lat_wgs84 lon_wgs84
#> 1  …KE.176/64895825d5de884fa20e297d#Unit1 Heracleum persicum …        NA  61.08302  22.38983
#> 2                           …JX.1594382#9 Hirundo rustica Lin…        NA  64.12716  23.99111
#> 3                          …JX.1594382#37 Pica pica (Linnaeus…        NA  64.12716  23.99111
#> 4                          …JX.1594382#49 Muscicapa striata (…        NA  64.12716  23.99111
#> 5                          …JX.1594382#39 Larus canus Linnaeu…        NA  64.12716  23.99111
#> 6                           …JX.1594382#5 Emberiza citrinella…        NA  64.12716  23.99111
#> 7                          …JX.1594382#31 Ficedula hypoleuca …        NA  64.12716  23.99111
#> 8                          …JX.1594382#41 Alauda arvensis Lin…        NA  64.12716  23.99111
#> 9                          …JX.1594382#21 Numenius arquata (L…        NA  64.12716  23.99111
#> 10                         …JX.1594382#29 Dendrocopos major (…        NA  64.12716  23.99111
#> ...with 0 more record and 7 more variables:
#> date_time, coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

Or set the global timezone option to set the timezone for the current session.

``` r
options(finbif_tz = "Etc/UTC")
```
This may be advisable for reproducibility or when working with multiple systems.
