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
#> Records available: 40410386
#> A data.frame [10 x 12]
#>               record_id      scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1        …JX.1315966#18             Trametes  1         65.08865  25.45157 2021-10-05 12:00:00
#> 2         …JX.1315966#9 Trichaptum fuscovio…  1         65.08865  25.45157 2021-10-05 12:00:00
#> 3        …JX.1315966#15                Fungi  1         65.08865  25.45157 2021-10-05 12:00:00
#> 4        …JX.1315966#12                Fungi  1         65.08865  25.45157 2021-10-05 12:00:00
#> 5         …JX.1315966#3  Lycogala epidendrum  1         65.08865  25.45157 2021-10-05 12:00:00
#> 6         …JX.1315966#6 Stereum sanguinolen…  1         65.08865  25.45157 2021-10-05 12:00:00
#> 7  …KE.176/615c07e7d5d…   Acherontia atropos  1         60.11016  25.01864 2021-10-05 12:00:00
#> 8         …JX.1315969#3      Rana temporaria  1         62.48525  21.75467 2021-10-05 12:00:00
#> 9        …JX.1315960#15   Puccinia absinthii  1         62.25399  25.71361 2021-10-05 12:00:00
#> 10        …JX.1315960#7    Puccinia tanaceti  1         62.2528   25.71456 2021-10-05 12:00:00
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
#> Records available: 71156
#> A data.frame [10 x 12]
#>              record_id scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1       …JX.1315699#12   Cygnus cygnus  3         60.56739  21.57188 2021-10-03 12:00:00
#> 2        …JX.1315624#7   Cygnus cygnus  2         63.77814  23.07286 2021-10-03 12:00:00
#> 3       …JX.1315190#24   Cygnus cygnus  2         60.42794  22.20052 2021-10-02 12:00:00
#> 4  …HR.3211/96901650-U   Cygnus cygnus  1         60.20356  25.18139 2021-10-02 12:00:00
#> 5  …HR.3211/96886383-U   Cygnus cygnus  1         62.91891  28.18733 2021-10-02 12:00:00
#> 6  …HR.3211/96873463-U   Cygnus cygnus  1         61.55599  25.95057 2021-10-02 12:00:00
#> 7       …JX.1315449#15   Cygnus cygnus  6         61.32296  28.56814 2021-10-02 10:40:00
#> 8       …JX.1315165#30   Cygnus cygnus  2         61.10535  21.55759 2021-10-02 08:25:00
#> 9        …JX.1296318#3   Cygnus cygnus  26        60.83174  26.44824 2021-09-30 12:00:00
#> 10 …HR.3211/96581760-U   Cygnus cygnus  1         61.46442  23.65117 2021-09-29 12:00:00
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
#> Records available: 101999
#> A data.frame [10 x 12]
#>              record_id scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1        …JX.1315856#9     Cygnus olor  8         60.42794  22.20052 2021-10-04 12:00:00
#> 2       …JX.1315724#21     Cygnus olor  6         60.44906  22.76845 2021-10-03 12:00:00
#> 3       …JX.1315701#36     Cygnus olor  8         60.42794  22.20052 2021-10-03 12:00:00
#> 4       …JX.1315699#24     Cygnus olor  8         60.56739  21.57188 2021-10-03 12:00:00
#> 5       …JX.1315699#12   Cygnus cygnus  3         60.56739  21.57188 2021-10-03 12:00:00
#> 6        …JX.1315624#7   Cygnus cygnus  2         63.77814  23.07286 2021-10-03 12:00:00
#> 7       …JX.1315190#24   Cygnus cygnus  2         60.42794  22.20052 2021-10-02 12:00:00
#> 8  …HR.3211/96901650-U   Cygnus cygnus  1         60.20356  25.18139 2021-10-02 12:00:00
#> 9  …HR.3211/96886383-U   Cygnus cygnus  1         62.91891  28.18733 2021-10-02 12:00:00
#> 10 …HR.3211/96873463-U   Cygnus cygnus  1         61.55599  25.95057 2021-10-02 12:00:00
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
#> Records available: 40410386
#> A data.frame [1001 x 12]
#>    record_id      scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1      …50#3 Exechiopsis fimbria…  1         65.01504  25.52607 2021-10-05 12:00:00
#> 2     …29#12 Depressaria badiella  1         62.92172  27.63335 2021-10-05 12:00:00
#> 3     …29#15 Depressaria daucella  1         62.92172  27.63335 2021-10-05 12:00:00
#> 4      …29#9   Epirrita autumnata  1         62.92172  27.63335 2021-10-05 12:00:00
#> 5      …29#6  Poecilocampa populi  1         62.92172  27.63335 2021-10-05 12:00:00
#> 6      …29#3      Xestia c-nigrum  1         62.92172  27.63335 2021-10-05 12:00:00
#> 7     …31#18 Agriopis aurantiaria  1         60.4528   22.40844 2021-10-04 12:00:00
#> 8      …31#3     Autographa gamma  1         60.4528   22.40844 2021-10-04 12:00:00
#> 9     …31#12 Chloroclysta sitera…  1         60.4528   22.40844 2021-10-04 12:00:00
#> 10    …31#15   Epirrita autumnata  1         60.4528   22.40844 2021-10-04 12:00:00
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
#> [1] 40410386
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
#> Records available: 4238
#> A data.frame [10 x 12]
#>               record_id scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1         …JX.1315490#3   Vulpes vulpes  1         65.05719  25.62634 2021-10-02 12:00:00
#> 2  …KE.176/6154c070d5d…   Vulpes vulpes  1         60.20882  24.90362 2021-09-29 12:00:00
#> 3         …JX.1294654#3   Vulpes vulpes  1         60.83805  21.37382 2021-09-24 05:30:00
#> 4   …HR.3211/95828765-U   Vulpes vulpes  1         60.76014  24.91448 2021-09-23 12:00:00
#> 5  …KE.176/614e5d42d5d…   Vulpes vulpes  1         60.11016  25.01864 2021-09-22 12:00:00
#> 6  …KE.176/614abfb4d5d…   Vulpes vulpes  1         60.13192  24.72079 2021-09-22 12:00:00
#> 7         …JX.1293417#7   Vulpes vulpes  1         60.82366  21.2978  2021-09-20 05:30:00
#> 8   …HR.3211/95510814-U   Vulpes vulpes  1         60.5      21.9     2021-09-19 12:00:00
#> 9       …JX.1293110#126   Vulpes vulpes  1         62.57362  28.58553 2021-09-18 12:00:00
#> 10      …JX.1272497#417   Vulpes vulpes  1         64.50244  29.98219 2021-09-17 12:00:00
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
#> Records available: 4238
#> A data.frame [10 x 12]
#>               record_id scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1         …JX.1315490#3   Vulpes vulpes  1         65.05719  25.62634 2021-10-02 12:00:00
#> 2  …KE.176/6154c070d5d…   Vulpes vulpes  1         60.20882  24.90362 2021-09-29 12:00:00
#> 3         …JX.1294654#3   Vulpes vulpes  1         60.83805  21.37382 2021-09-24 05:30:00
#> 4   …HR.3211/95828765-U   Vulpes vulpes  1         60.76014  24.91448 2021-09-23 12:00:00
#> 5  …KE.176/614e5d42d5d…   Vulpes vulpes  1         60.11016  25.01864 2021-09-22 12:00:00
#> 6  …KE.176/614abfb4d5d…   Vulpes vulpes  1         60.13192  24.72079 2021-09-22 12:00:00
#> 7         …JX.1293417#7   Vulpes vulpes  1         60.82366  21.2978  2021-09-20 05:30:00
#> 8   …HR.3211/95510814-U   Vulpes vulpes  1         60.5      21.9     2021-09-19 12:00:00
#> 9       …JX.1293110#126   Vulpes vulpes  1         62.57362  28.58553 2021-09-18 12:00:00
#> 10      …JX.1272497#417   Vulpes vulpes  1         64.50244  29.98219 2021-09-17 12:00:00
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
#> Records available: 317
#> A data.frame [10 x 2]
#>    municipality n_species
#> 1       Kouvola      1328
#> 2     Virolahti      1932
#> 3      Rääkkylä      1346
#> 4   Kemiönsaari      1963
#> 5      Parainen      1765
#> 6         Hanko      1895
#> 7      Helsinki      1937
#> 8     Raasepori      1860
#> 9         Kotka      1635
#> 10       Kuopio      1319

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
#> Records available: 40410386
#> A data.frame [10 x 11]
#>               record_id      scientific_name abundance lat_wgs84 lon_wgs84 coordinates_uncertainty
#> 1        …JX.1315966#18             Trametes  1         65.08865  25.45157  1                     
#> 2         …JX.1315966#9 Trichaptum fuscovio…  1         65.08865  25.45157  1                     
#> 3        …JX.1315966#15                Fungi  1         65.08865  25.45157  1                     
#> 4        …JX.1315966#12                Fungi  1         65.08865  25.45157  1                     
#> 5         …JX.1315966#3  Lycogala epidendrum  1         65.08865  25.45157  1                     
#> 6         …JX.1315966#6 Stereum sanguinolen…  1         65.08865  25.45157  1                     
#> 7  …KE.176/615c07e7d5d…   Acherontia atropos  1         60.11016  25.01864  50000                 
#> 8         …JX.1315969#3      Rana temporaria  1         62.48525  21.75467  1                     
#> 9        …JX.1315960#15   Puccinia absinthii  1         62.25399  25.71361  1                     
#> 10        …JX.1315960#7    Puccinia tanaceti  1         62.2528   25.71456  1                     
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
#> Records available: 40410386
#> A data.frame [10 x 12]
#>               record_id      scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1        …JX.1315966#18             Trametes  1         65.08865  25.45157 2021-10-05 12:00:00
#> 2         …JX.1315966#9 Trichaptum fuscovio…  1         65.08865  25.45157 2021-10-05 12:00:00
#> 3        …JX.1315966#15                Fungi  1         65.08865  25.45157 2021-10-05 12:00:00
#> 4        …JX.1315966#12                Fungi  1         65.08865  25.45157 2021-10-05 12:00:00
#> 5         …JX.1315966#3  Lycogala epidendrum  1         65.08865  25.45157 2021-10-05 12:00:00
#> 6         …JX.1315966#6 Stereum sanguinolen…  1         65.08865  25.45157 2021-10-05 12:00:00
#> 7  …KE.176/615c07e7d5d…   Acherontia atropos  1         60.11016  25.01864 2021-10-05 12:00:00
#> 8         …JX.1315969#3      Rana temporaria  1         62.48525  21.75467 2021-10-05 12:00:00
#> 9        …JX.1315960#15   Puccinia absinthii  1         62.25399  25.71361 2021-10-05 12:00:00
#> 10        …JX.1315960#7    Puccinia tanaceti  1         62.2528   25.71456 2021-10-05 12:00:00
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
#> Records available: 40410386
#> A data.frame [10 x 12]
#>               record_id      scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1        …JX.1315966#18             Trametes  1         65.08865  25.45157 2021-10-05 09:00:00
#> 2         …JX.1315966#9 Trichaptum fuscovio…  1         65.08865  25.45157 2021-10-05 09:00:00
#> 3        …JX.1315966#15                Fungi  1         65.08865  25.45157 2021-10-05 09:00:00
#> 4        …JX.1315966#12                Fungi  1         65.08865  25.45157 2021-10-05 09:00:00
#> 5         …JX.1315966#3  Lycogala epidendrum  1         65.08865  25.45157 2021-10-05 09:00:00
#> 6         …JX.1315966#6 Stereum sanguinolen…  1         65.08865  25.45157 2021-10-05 09:00:00
#> 7  …KE.176/615c07e7d5d…   Acherontia atropos  1         60.11016  25.01864 2021-10-05 09:00:00
#> 8         …JX.1315969#3      Rana temporaria  1         62.48525  21.75467 2021-10-05 09:00:00
#> 9        …JX.1315960#15   Puccinia absinthii  1         62.25399  25.71361 2021-10-05 09:00:00
#> 10        …JX.1315960#7    Puccinia tanaceti  1         62.2528   25.71456 2021-10-05 09:00:00
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
