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
#> Records available: 280421
#> A data.frame [10 x 3]
#>      scientific_name life_stage sex
#> 1  Falco tinnunculus         NA  NA
#> 2  Falco tinnunculus         NA  NA
#> 3  Falco tinnunculus         NA  NA
#> 4  Falco tinnunculus         NA  NA
#> 5  Falco tinnunculus         NA  NA
#> 6  Falco tinnunculus         NA  NA
#> 7  Falco tinnunculus         NA  NA
#> 8  Falco tinnunculus         NA  NA
#> 9  Falco tinnunculus         NA  NA
#> 10 Falco tinnunculus         NA  NA

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
#> Records available: 34696897
#> A data.frame [10 x 13]
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
#> Records available: 57007
#> A data.frame [10 x 12]
#>               record_id scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1    KE.67/9403350#Unit   Cygnus cygnus  1         60.41667  16       1997-04-01 13:00:00
#> 2          MHU.29327372   Cygnus cygnus  1         63.37022  30.37826                <NA>
#> 3  KE.176/58c7db302d0e…   Cygnus cygnus  1         61.082    27.78355 2017-04-08 12:00:00
#> 4    KE.67/9069501#Unit   Cygnus cygnus  1         52.71667  1.55     1997-01-03 14:00:00
#> 5          MHU.12651157   Cygnus cygnus  1         61.21489  23.36719 2006-06-12 12:00:00
#> 6    KE.67/9465507#Unit   Cygnus cygnus  1         61.8      22.76667 2000-03-22 12:00:00
#> 7    KE.67/9607357#Unit   Cygnus cygnus  1         63.13333  22.43333 2003-06-07 12:00:00
#> 8  A.94EBF006-FAD1-482…   Cygnus cygnus  1         61.72481  24.17242                <NA>
#> 9          MHU.30415803   Cygnus cygnus  1         60.23968  25.79487                <NA>
#> 10         MHU.18161417   Cygnus cygnus  1         62.27627  23.59633 1994-04-30 19:50:00
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
#> Records available: 57007
#> A data.frame [10 x 12]
#>           record_id scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1       MHU.2981587   Cygnus cygnus  6000      64.4     -14.54    1995-07-05 15:00:00
#> 2      MHU.29480894   Cygnus cygnus  2010      64.54597  27.88859 2010-01-01 12:00:00
#> 3      MHU.28815250   Cygnus cygnus  1500            NA        NA 2003-04-18 15:00:00
#> 4      MHU.28815110   Cygnus cygnus  1200            NA        NA 2003-04-16 15:00:00
#> 5  KE.8_1209789#134   Cygnus cygnus  1064      62.50172  21.3326  2017-11-04 08:40:00
#> 6    KE.8_1186061#2   Cygnus cygnus  722       60.9717   26.48918 2012-11-04 09:00:00
#> 7  KE.8_1071847#190   Cygnus cygnus  673       60.76462  26.09601 2012-11-07 08:00:00
#> 8       MHU.1072224   Cygnus cygnus  645       64.84588  25.62653 2005-11-05 12:00:00
#> 9       JX.111286#2   Cygnus cygnus  645       64.84588  25.62653 2005-11-05 09:00:00
#> 10     MHU.29306332   Cygnus cygnus  641       62.27627  23.59633 2008-11-20 12:00:00
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
finbif_occurrence("Cygnus olor", order_by = c("municipality", "-abundance"))
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 24344
#> A data.frame [10 x 12]
#>           record_id scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1       JX.133519#2     Cygnus olor  4         61.13908  23.93331 2007-11-04 09:00:00
#> 2       JX.133468#2     Cygnus olor  3         61.13908  23.93331 1990-11-04 09:00:00
#> 3        JX.81211#2     Cygnus olor  3         61.22435  23.73872 1990-11-14 11:00:00
#> 4  KE.8_1106444#139     Cygnus olor  2         61.16607  23.91207 2013-11-09 08:30:00
#> 5   KE.8_1025765#50     Cygnus olor  1         61.22435  23.73872 2007-03-01 10:00:00
#> 6      MHU.28634404     Cygnus olor  4         61.13908  23.93331 2007-11-04 12:00:00
#> 7       JX.135405#3     Cygnus olor  6         61.2287   23.92458 2002-11-01 12:00:00
#> 8       JX.135402#3     Cygnus olor  3         61.2287   23.92458 2000-12-22 12:00:00
#> 9      MHU.12868652     Cygnus olor  1         61.2287   23.92458 2006-03-11 12:00:00
#> 10     MHU.29307741     Cygnus olor  1         61.2287   23.92458 2010-05-11 12:00:00
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
#> Records available: 34696897
#> A data.frame [10 x 12]
#>             record_id      scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1        MHU.28617380     Poecile montanus  6         60.07116  24.39247 2005-11-05 12:00:00
#> 2         JX.544412#2      Macaria wauaria  2         60.90432  26.7206  2007-01-01 12:00:00
#> 3  KE.67/4941088#Unit          Parus major  1         60.43333  22.13333 1978-06-14 12:00:00
#> 4        JX.1099912#4          Lutra lutra  1         64.15453  28.28361 2020-02-16 12:00:00
#> 5          MKC.746569 Alchemilla glaucesc…  1         60.24286  24.01753 1906-06-08 12:00:00
#> 6  KE.67/6848719#Unit Phylloscopus trochi…  1         60.48775  26.93416 1994-08-31 12:00:00
#> 7         JX.468110#2      Mythimna impura  1         60.1622   21.49994 2002-01-01 12:00:00
#> 8        JX.115482#17       Periparus ater  5         62.23926  21.70279 2004-11-06 09:00:00
#> 9  KE.67/8872635#Unit Acrocephalus scirpa…  1         60.21319  24.99189 1986-08-11 07:00:00
#> 10        JX.241328#2     Colocasia coryli  1         62.24982  27.47792 2014-06-09 12:00:00
#> ...with 0 more records and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>
