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
#> Records available: 34700400
#> A data.frame [10 x 12]
#>    record_id     scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1      9#111      Aglais urticae  16        60.37011  24.72526 2020-04-22 15:00:00
#> 2       8#19               Loxia  1         61.13068  21.65088 2020-04-22 07:20:00
#> 3        8#4    Poecile montanus  1         61.13068  21.65088 2020-04-22 07:20:00
#> 4       8#16      Turdus iliacus  1         61.13068  21.65088 2020-04-22 07:20:00
#> 5        8#7  Erithacus rubecula  1         61.13068  21.65088 2020-04-22 07:20:00
#> 6       8#13 Cyanistes caeruleus  1         61.13068  21.65088 2020-04-22 07:20:00
#> 7       8#10       Spinus spinus  3         61.13068  21.65088 2020-04-22 07:20:00
#> 8       7#13   Turdus philomelos  1         61.13305  21.65151 2020-04-22 06:30:00
#> 9       7#25     Tringa ochropus  1         61.13305  21.65151 2020-04-22 06:30:00
#> 10       7#7 Garrulus glandarius  1         61.13305  21.65151 2020-04-22 06:30:00
#> ...with 0 more records and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
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
#>  [7] "coordinateUncertaintyInMeters" "hasIssues"                     "requires_verification"        
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
#> Records available: 57007
#> A data.frame [10 x 12]
#>               record_id scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1  KE.176/5e9dd5782d0e…   Cygnus cygnus  1         62.79256  30.96101 2020-04-20 12:00:00
#> 2        JX.1108556#265   Cygnus cygnus  2         64.2081   29.03035 2020-04-19 12:00:00
#> 3         JX.1110250#40   Cygnus cygnus  20        63.12255  24.32082 2020-04-19 05:00:00
#> 4          JX.1110117#3   Cygnus cygnus  2         65.87333  24.28142 2020-04-19 17:00:00
#> 5         JX.1110052#46   Cygnus cygnus  2         61.13841  21.64607 2020-04-19 06:35:00
#> 6         JX.1110024#10   Cygnus cygnus  2         60.32418  22.39303 2020-04-18 12:00:00
#> 7          JX.1110008#3   Cygnus cygnus  10        64.17427  25.9137  2020-04-17 12:00:00
#> 8         JX.1109958#52   Cygnus cygnus  2         61.32291  28.56818 2020-04-17 05:45:00
#> 9          JX.1109939#4   Cygnus cygnus  2         61.2604   28.71105 2020-04-17 12:27:00
#> 10         JX.1109952#4   Cygnus cygnus  4         61.2641   28.70687 2020-04-16 11:33:00
#> ...with 0 more records and 6 more variables:
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
#> Records available: 81351
#> A data.frame [10 x 12]
#>               record_id scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1  KE.176/5e9dd5782d0e…   Cygnus cygnus  1         62.79256  30.96101 2020-04-20 12:00:00
#> 2        JX.1108556#265   Cygnus cygnus  2         64.2081   29.03035 2020-04-19 12:00:00
#> 3         JX.1110250#40   Cygnus cygnus  20        63.12255  24.32082 2020-04-19 05:00:00
#> 4         JX.1110172#49     Cygnus olor  2         61.0113   21.38515 2020-04-19 12:00:00
#> 5          JX.1110117#3   Cygnus cygnus  2         65.87333  24.28142 2020-04-19 17:00:00
#> 6         JX.1110052#46   Cygnus cygnus  2         61.13841  21.64607 2020-04-19 06:35:00
#> 7    HR.3211/42456319-U     Cygnus olor  1         60.17978  24.93999 2020-04-18 12:00:00
#> 8         JX.1110024#10   Cygnus cygnus  2         60.32418  22.39303 2020-04-18 12:00:00
#> 9         JX.1109909#31     Cygnus olor  20        61.10578  21.44994 2020-04-18 07:35:00
#> 10         JX.1110008#3   Cygnus cygnus  10        64.17427  25.9137  2020-04-17 12:00:00
#> ...with 0 more records and 6 more variables:
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
#> Records available: 34696846
#> A data.frame [1001 x 12]
#>               record_id      scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1          JX.1110588#7            Aglais io  2         63.13815  21.56879 2020-04-21 12:00:00
#> 2          JX.1110588#4       Aglais urticae  6         63.13815  21.56879 2020-04-21 12:00:00
#> 3         JX.1110588#10    Nymphalis antiopa  1         63.13815  21.56879 2020-04-21 12:00:00
#> 4          JX.1110427#4 Noccaea caerulescen…  1         60.72342  24.84861 2020-04-21 12:00:00
#> 5  KE.176/5e5bb8932d0e…       Aglais urticae  1         62.94     26.794   2020-04-21 12:00:00
#> 6        JX.1110587#148  Phasianus colchicus  1         60.4088   22.20608 2020-04-21 12:00:00
#> 7         JX.1110587#82            Pica pica  7         60.4088   22.20608 2020-04-21 12:00:00
#> 8        JX.1110587#121        Ardea cinerea  1         60.4088   22.20608 2020-04-21 12:00:00
#> 9        JX.1110587#112     Larus argentatus  3         60.4088   22.20608 2020-04-21 12:00:00
#> 10       JX.1110587#115          Picus canus  1         60.4088   22.20608 2020-04-21 12:00:00
#> ...with 991 more records and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

You can see how many records are available for a given request, without
retrieving any records, by setting `count_only = TRUE`.

```r
finbif_occurrence(count_only = TRUE)
#> [1] 34696846
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
#> Records available: 3326
#> A data.frame [10 x 12]
#>               record_id scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1         JX.1110052#67   Vulpes vulpes  1         61.13841  21.64607 2020-04-19 06:35:00
#> 2         JX.1109239#13   Vulpes vulpes  1         66.82291  28.39749 2020-04-13 12:00:00
#> 3          JX.1108569#4   Vulpes vulpes  1         65.07722  25.5018  2020-04-11 12:00:00
#> 4          JX.1109741#4   Vulpes vulpes  1         60.30568  24.70511 2020-04-08 12:00:00
#> 5          JX.1107461#8   Vulpes vulpes  1         60.11628  23.47399 2020-04-05 12:00:00
#> 6  KE.176/5e98b2d02d0e…   Vulpes vulpes  1         61.53415  25.51793 2020-04-03 12:00:00
#> 7         JX.1105990#19   Vulpes vulpes  1         60.42794  22.20052 2020-03-31 12:00:00
#> 8          JX.1104874#4   Vulpes vulpes  1         62.61875  29.66306 2020-03-22 12:00:00
#> 9         JX.1104505#19   Vulpes vulpes  1         60.10224  23.5459  2020-03-22 12:00:00
#> 10         JX.1103437#4   Vulpes vulpes  1         60.55732  22.25711 2020-03-14 09:00:00
#> ...with 0 more records and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
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
#> Records available: 3326
#> A data.frame [10 x 12]
#>               record_id scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1         JX.1110052#67   Vulpes vulpes  1         61.13841  21.64607 2020-04-19 06:35:00
#> 2         JX.1109239#13   Vulpes vulpes  1         66.82291  28.39749 2020-04-13 12:00:00
#> 3          JX.1108569#4   Vulpes vulpes  1         65.07722  25.5018  2020-04-11 12:00:00
#> 4          JX.1109741#4   Vulpes vulpes  1         60.30568  24.70511 2020-04-08 12:00:00
#> 5          JX.1107461#8   Vulpes vulpes  1         60.11628  23.47399 2020-04-05 12:00:00
#> 6  KE.176/5e98b2d02d0e…   Vulpes vulpes  1         61.53415  25.51793 2020-04-03 12:00:00
#> 7         JX.1105990#19   Vulpes vulpes  1         60.42794  22.20052 2020-03-31 12:00:00
#> 8          JX.1104874#4   Vulpes vulpes  1         62.61875  29.66306 2020-03-22 12:00:00
#> 9         JX.1104505#19   Vulpes vulpes  1         60.10224  23.5459  2020-03-22 12:00:00
#> 10         JX.1103437#4   Vulpes vulpes  1         60.55732  22.25711 2020-03-14 09:00:00
#> ...with 0 more records and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

By setting the argument, `on_check_fail` to `"error"` (the default is `"warn"`),
you can elevate the warnings to errors and the request will fail if any of the
taxa are not found in the FinBIF database.

```r
finbif_occurrence("Vulpes vulpes", "Moomin", on_check_fail = "error")
#> Error: Cannot find taxa: Moomin
```
This can be a useful strategy if you are using `{finbif}` non-interactively
(in a script), and you do not want to proceed if any of your taxon names are
wrong or misspelled.

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
#> Records available: 34696846
#> A data.frame [10 x 11]
#>               record_id      scientific_name abundance lat_wgs84 lon_wgs84 coordinates_uncertainty
#> 1          JX.1110588#7            Aglais io  2         63.13815  21.56879  1000                  
#> 2          JX.1110588#4       Aglais urticae  6         63.13815  21.56879  1000                  
#> 3         JX.1110588#10    Nymphalis antiopa  1         63.13815  21.56879  1000                  
#> 4          JX.1110427#4 Noccaea caerulescen…  1         60.72342  24.84861  1                     
#> 5  KE.176/5e5bb8932d0e…       Aglais urticae  1         62.94     26.794    10000                 
#> 6        JX.1110587#148  Phasianus colchicus  1         60.4088   22.20608  10000                 
#> 7         JX.1110587#82            Pica pica  7         60.4088   22.20608  10000                 
#> 8        JX.1110587#121        Ardea cinerea  1         60.4088   22.20608  10000                 
#> 9        JX.1110587#112     Larus argentatus  3         60.4088   22.20608  10000                 
#> 10       JX.1110587#115          Picus canus  1         60.4088   22.20608  10000                 
#> ...with 0 more records and 5 more variables:
#> any_issues, requires_verification, requires_identification, record_reliability, record_quality

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
#> Records available: 34696846
#> A data.frame [10 x 12]
#>               record_id      scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1          JX.1110588#7            Aglais io  2         63.13815  21.56879 2020-04-21 12:00:00
#> 2          JX.1110588#4       Aglais urticae  6         63.13815  21.56879 2020-04-21 12:00:00
#> 3         JX.1110588#10    Nymphalis antiopa  1         63.13815  21.56879 2020-04-21 12:00:00
#> 4          JX.1110427#4 Noccaea caerulescen…  1         60.72342  24.84861 2020-04-21 12:00:00
#> 5  KE.176/5e5bb8932d0e…       Aglais urticae  1         62.94     26.794   2020-04-21 12:00:00
#> 6        JX.1110587#148  Phasianus colchicus  1         60.4088   22.20608 2020-04-21 12:00:00
#> 7         JX.1110587#82            Pica pica  7         60.4088   22.20608 2020-04-21 12:00:00
#> 8        JX.1110587#121        Ardea cinerea  1         60.4088   22.20608 2020-04-21 12:00:00
#> 9        JX.1110587#112     Larus argentatus  3         60.4088   22.20608 2020-04-21 12:00:00
#> 10       JX.1110587#115          Picus canus  1         60.4088   22.20608 2020-04-21 12:00:00
#> ...with 0 more records and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

#### Timezone output
The timezone of the calculated `date_time` variable is determined by the
timezone of your operating system.

```r
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
#> Records available: 34696846
#> A data.frame [10 x 12]
#>               record_id      scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1          JX.1110588#7            Aglais io  2         63.13815  21.56879 2020-04-21 09:00:00
#> 2          JX.1110588#4       Aglais urticae  6         63.13815  21.56879 2020-04-21 09:00:00
#> 3         JX.1110588#10    Nymphalis antiopa  1         63.13815  21.56879 2020-04-21 09:00:00
#> 4          JX.1110427#4 Noccaea caerulescen…  1         60.72342  24.84861 2020-04-21 09:00:00
#> 5  KE.176/5e5bb8932d0e…       Aglais urticae  1         62.94     26.794   2020-04-21 09:00:00
#> 6        JX.1110587#148  Phasianus colchicus  1         60.4088   22.20608 2020-04-21 09:00:00
#> 7         JX.1110587#82            Pica pica  7         60.4088   22.20608 2020-04-21 09:00:00
#> 8        JX.1110587#121        Ardea cinerea  1         60.4088   22.20608 2020-04-21 09:00:00
#> 9        JX.1110587#112     Larus argentatus  3         60.4088   22.20608 2020-04-21 09:00:00
#> 10       JX.1110587#115          Picus canus  1         60.4088   22.20608 2020-04-21 09:00:00
#> ...with 0 more records and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

Or set the global timezone option to set the timezone for the current session.

```r
options(finbif_tz = "Etc/UTC")
```
This may be advisable for reproducibility or when working with multiple systems.
