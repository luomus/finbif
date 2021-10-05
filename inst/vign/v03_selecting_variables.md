---
title: "Selecting and ordering variables"
author: "William K. Morris"
output: 
  rmarkdown::html_vignette:
    toc: true
vignette: >
  %\VignetteIndexEntry{3. Selecting and ordering variables}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



When requesting data using `{finbif}` you can select from among (or order by)
many variables (properties of the occurrence records). The default set of
variables returned is a small subset of those available. Note that not all
variables are available for all records. See `?variables` for details.

## Selecting variables
### Limiting variables
To retrieve a limited set of variables from FinBIF simply specify the desired
variables in the `select` argument.

```r
finbif_occurrence(
  genus  = "Falco",
  select = c("scientific_name", "life_stage", "sex")
)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 315462
#> A data.frame [10 x 3]
#>      scientific_name life_stage sex
#> 1  Falco columbarius         NA  NA
#> 2  Falco columbarius         NA  NA
#> 3     Falco subbuteo         NA  NA
#> 4  Falco tinnunculus         NA  NA
#> 5  Falco tinnunculus         NA  NA
#> 6     Falco subbuteo         NA  NA
#> 7              Falco         NA  NA
#> 8  Falco tinnunculus         NA  NA
#> 9  Falco columbarius         NA  NA
#> 10             Falco         NA  NA

```

</details>
<br>

### Extra variables
To get extra variables as well as the default set, specify the extra variables
in addition to the keyword `"default_vars"`.

```r
finbif_occurrence(select = c("default_vars", "life_stage"))
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 40410386
#> A data.frame [10 x 13]
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
#> ...with 0 more records and 7 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality, life_stage

```

</details>
<br>

## Ordering
You can change the order of occurrence records before they are fetched from the
server by using the `order_by` argument. The default ordering is `date_start`
descending, then `load_date` descending, then `reported_name`.

### Ascending order
By default occurrence records are ordered by variables in ascending order.

```r
finbif_occurrence("Cygnus cygnus", order_by = "abundance")
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 71156
#> A data.frame [10 x 12]
#>               record_id scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1   …KE.67/9403350#Unit   Cygnus cygnus  1         60.41667  16       1997-04-01 13:00:00
#> 2         …MHU.29327372   Cygnus cygnus  1         63.37022  30.37826                <NA>
#> 3  …KE.176/58c7db302d0…   Cygnus cygnus  1         61.082    27.78355 2017-04-08 12:00:00
#> 4          …MHU.2529766   Cygnus cygnus  1         64.94099  27.52532 2007-06-09 12:00:00
#> 5   …KE.67/9069501#Unit   Cygnus cygnus  1         52.71667  1.55     1997-01-03 14:00:00
#> 6         …MHU.12651157   Cygnus cygnus  1         61.21489  23.36719 2006-06-12 12:00:00
#> 7         …MHU.21539922   Cygnus cygnus  1         60.42008  22.44084 2009-03-15 12:00:00
#> 8   …KE.67/9465507#Unit   Cygnus cygnus  1         61.8      22.76667 2000-03-22 12:00:00
#> 9   …KE.67/9607357#Unit   Cygnus cygnus  1         63.13333  22.43333 2003-06-07 12:00:00
#> 10  …HR.3211/71804082-U   Cygnus cygnus  1         60.22502  25.00972 2021-03-21 12:00:00
#> ...with 0 more records and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

### Descending order
You can switch to descending order by prefixing the variable with a dash.

```r
finbif_occurrence("Cygnus cygnus", order_by = "-abundance")
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 71156
#> A data.frame [10 x 12]
#>               record_id scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1          …MHU.2981587   Cygnus cygnus  6000      64.4     -14.54    1995-07-05 15:00:00
#> 2         …MHU.29480894   Cygnus cygnus  2010      64.54597  27.88859 2010-01-01 12:00:00
#> 3  …HR.3691/OBS6046423…   Cygnus cygnus  1753      64.5074   24.2789  2018-04-23 12:00:00
#> 4  …HR.3691/OBS6635688…   Cygnus cygnus  1600      65.9879   24.0634  2018-10-06 13:00:00
#> 5         …MHU.28815250   Cygnus cygnus  1500            NA        NA 2003-04-18 15:00:00
#> 6  …HR.3691/OBS6713538…   Cygnus cygnus  1361      64.7166   24.5319  2018-10-27 12:00:00
#> 7         …MHU.28815110   Cygnus cygnus  1200            NA        NA 2003-04-16 15:00:00
#> 8  …HR.3691/OBS6015318…   Cygnus cygnus  1145      64.5074   24.2789  2018-04-18 12:00:00
#> 9  …HR.3691/OBS6023666…   Cygnus cygnus  1125      64.5074   24.2789  2018-04-19 12:00:00
#> 10    …KE.8_1209789#134   Cygnus cygnus  1064      62.50172  21.3326  2017-11-04 08:40:00
#> ...with 0 more records and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

### Multiple variables
You can specify multiple variables to order by. Sorting primacy is from left to
right.

```r
finbif_occurrence("Cygnus olor", order_by = c("municipality_id", "-abundance"))
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 30843
#> A data.frame [10 x 12]
#>    record_id scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1  …12015253     Cygnus olor  2500            NA        NA 2008-08-17 15:00:00
#> 2  …13633876     Cygnus olor  1500            NA        NA 2008-10-03 15:00:00
#> 3    …780984     Cygnus olor  1300            NA        NA 2006-02-14 14:00:00
#> 4    …781416     Cygnus olor  1300            NA        NA 2006-02-14 14:00:00
#> 5   …2062563     Cygnus olor  1200            NA        NA 2006-12-19 14:00:00
#> 6  …10352894     Cygnus olor  1100            NA        NA 2008-06-28 15:00:00
#> 7    …927797     Cygnus olor  1050            NA        NA 2006-04-05 15:00:00
#> 8   …3110953     Cygnus olor  960             NA        NA 2007-10-06 15:00:00
#> 9  …26309317     Cygnus olor  900             NA        NA 2009-07-08 15:00:00
#> 10  …1260385     Cygnus olor  800       58.66     23.57    2006-04-26 18:00:00
#> ...with 0 more records and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

### Random sample
You can also request a random sample (random order) of occurrence records by
setting the `sample` argument to `TRUE`.

```r
finbif_occurrence(sample = TRUE)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 40410449
#> A data.frame [10 x 12]
#>               record_id      scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1   …KE.67/2798389#Unit  Emberiza citrinella  1         63.1745   23.85393 2005-06-26 12:00:00
#> 2   …KE.67/5096145#Unit      Hirundo rustica  1         61.31215  23.78458 2000-08-31 20:00:00
#> 3           …JX.98066#3            Pica pica  7         61.16842  25.78921 1969-12-29 09:00:00
#> 4         …MKC.30782842 Vaccinium uliginosum  1         60.71347  21.67181 2016-10-06 12:00:00
#> 5        …JX.1041885#63     Gimnomera tarsea  1         67.568    26.8804  2012-07-06 12:00:00
#> 6          …JX.751150#2 Brachylomia viminal…  1         61.09245  21.47795 1974-09-20 12:00:00
#> 7          …MKC.5700111            Taraxacum  1         62.56906  24.48739 1969-01-01 12:00:00
#> 8  …KE.67/10030682#Unit     Larus argentatus  1         60.75066  26.86027 2004-07-11 12:00:00
#> 9         …MKC.24352916         Rubus idaeus  1         62.81964  23.41582 2008-07-03 12:00:00
#> 10  …KE.67/7670229#Unit      Curruca curruca  1         59.93333  24.4     1979-06-09 04:00:00
#> ...with 0 more records and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>
