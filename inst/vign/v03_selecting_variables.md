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
#> Records available: 342668
#> A data.frame [10 x 3]
#>         scientific_name life_stage sex
#> 1  Falco columbarius L…         NA  NA
#> 2  Falco tinnunculus L…         NA  NA
#> 3  Falco tinnunculus L…         NA  NA
#> 4  Falco subbuteo Linn…         NA  NA
#> 5  Falco tinnunculus L…         NA  NA
#> 6  Falco subbuteo Linn…         NA  NA
#> 7  Falco tinnunculus L…         NA  NA
#> 8  Falco tinnunculus L…         NA  NA
#> 9  Falco tinnunculus L…         NA  NA
#> 10 Falco subbuteo Linn…         NA  NA

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
#> Records available: 47159747
#> A data.frame [10 x 13]
#>                                 record_id      scientific_name abundance lat_wgs84 lon_wgs84
#> 1                           …JX.1594385#3 Sciurus vulgaris Li…  1         60.23584  25.05693
#> 2  …KE.176/64895825d5de884fa20e297d#Unit1 Heracleum persicum …        NA  61.08302  22.38983
#> 3                           …JX.1594382#9 Hirundo rustica Lin…        NA  64.12716  23.99111
#> 4                          …JX.1594382#37 Pica pica (Linnaeus…        NA  64.12716  23.99111
#> 5                          …JX.1594382#49 Muscicapa striata (…        NA  64.12716  23.99111
#> 6                          …JX.1594382#39 Larus canus Linnaeu…        NA  64.12716  23.99111
#> 7                           …JX.1594382#5 Emberiza citrinella…        NA  64.12716  23.99111
#> 8                          …JX.1594382#31 Ficedula hypoleuca …        NA  64.12716  23.99111
#> 9                          …JX.1594382#41 Alauda arvensis Lin…        NA  64.12716  23.99111
#> 10                         …JX.1594382#21 Numenius arquata (L…        NA  64.12716  23.99111
#> ...with 0 more record and 8 more variables:
#> date_time, coordinates_uncertainty, any_issues, requires_verification, requires_identification,
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
#> Records available: 95730
#> A data.frame [10 x 12]
#>                     record_id      scientific_name abundance lat_wgs84 lon_wgs84
#> 1         …KE.67/9403350#Unit Cygnus cygnus (Linn…  1         60.41667  16      
#> 2  …HR.3691/OBS810893905_Unit Cygnus cygnus (Linn…        NA  61.56563  29.56771
#> 3             …JX.1026351#259 Cygnus cygnus (Linn…  1         65.89416  28.90961
#> 4               …JX.1025175#3 Cygnus cygnus (Linn…  1         61.83248  23.40816
#> 5         …KE.67/9069501#Unit Cygnus cygnus (Linn…  1         52.71667  1.55    
#> 6               …JX.1252188#3 Cygnus cygnus (Linn…        NA  60.17258  24.27095
#> 7         …KE.67/9465507#Unit Cygnus cygnus (Linn…  1         61.8      22.76667
#> 8         …KE.67/9607357#Unit Cygnus cygnus (Linn…  1         63.13333  22.43333
#> 9               …MHU.10961285 Cygnus cygnus (Linn…        NA  64.97813  24.74932
#> 10 …HR.3691/OBS886590077_Unit Cygnus cygnus (Linn…        NA  61.27566  22.557  
#>              date_time
#> 1  1997-04-01 13:00:00
#> 2  2007-05-18 12:00:00
#> 3  2013-06-24 03:12:00
#> 4  2014-06-08 03:45:00
#> 5  1997-01-03 14:00:00
#> 6  2021-05-23 06:15:00
#> 7  2000-03-22 12:00:00
#> 8  2003-06-07 12:00:00
#> 9  2008-07-12 12:00:00
#> 10 2020-03-31 12:00:00
#> ...with 0 more record and 6 more variables:
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
#> Records available: 95730
#> A data.frame [10 x 12]
#>                      record_id      scientific_name abundance lat_wgs84 lon_wgs84
#> 1                 …MHU.2981587 Cygnus cygnus (Linn…  6000      64.4     -14.54   
#> 2  …HR.3691/OBS1101526155_Unit Cygnus cygnus (Linn…  1760      62.16389  21.45786
#> 3   …HR.3691/OBS604642304_Unit Cygnus cygnus (Linn…  1753      64.50736  24.27894
#> 4   …HR.3691/OBS663568887_Unit Cygnus cygnus (Linn…  1600      65.98787  24.06341
#> 5                …MHU.28815250 Cygnus cygnus (Linn…  1500            NA        NA
#> 6   …HR.3691/OBS671353848_Unit Cygnus cygnus (Linn…  1361      64.71656  24.53188
#> 7                …JX.1357345#5 Cygnus cygnus (Linn…  1300      64.8465   25.2883 
#> 8                …JX.1398409#3 Cygnus cygnus (Linn…  1280      64.8448   25.2816 
#> 9                …MHU.28815110 Cygnus cygnus (Linn…  1200            NA        NA
#> 10 …HR.3691/OBS1119137190_Unit Cygnus cygnus (Linn…  1163      64.71656  24.53188
#> ...with 0 more record and 7 more variables:
#> date_time, coordinates_uncertainty, any_issues, requires_verification, requires_identification,
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
#> Records available: 42951
#> A data.frame [10 x 12]
#>    record_id      scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1  …12015253 Cygnus olor (J.F. G…  2500            NA        NA 2008-08-17 12:00:00
#> 2  …13633876 Cygnus olor (J.F. G…  1500            NA        NA 2008-10-03 12:00:00
#> 3    …781416 Cygnus olor (J.F. G…  1300            NA        NA 2006-02-14 12:00:00
#> 4    …780984 Cygnus olor (J.F. G…  1300            NA        NA 2006-02-14 12:00:00
#> 5   …2062563 Cygnus olor (J.F. G…  1200            NA        NA 2006-12-19 12:00:00
#> 6  …10352894 Cygnus olor (J.F. G…  1100            NA        NA 2008-06-28 12:00:00
#> 7    …927797 Cygnus olor (J.F. G…  1050            NA        NA 2006-04-05 12:00:00
#> 8   …3110953 Cygnus olor (J.F. G…  960             NA        NA 2007-10-06 12:00:00
#> 9  …26309317 Cygnus olor (J.F. G…  900             NA        NA 2009-07-08 12:00:00
#> 10  …1260385 Cygnus olor (J.F. G…  800       58.66     23.57    2006-04-26 18:00:00
#> ...with 0 more record and 6 more variables:
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
#> Records available: 47159747
#> A data.frame [10 x 12]
#>                      record_id      scientific_name abundance lat_wgs84 lon_wgs84
#> 1          …KE.67/5632630#Unit Ficedula hypoleuca …  1         61.74637  22.77479
#> 2                   …MHU.40397      Prunus padus L.        NA  60.28016  20.25637
#> 3                 …JX.775148#3 Yezognophos vittari…  1         60.44245  27.006  
#> 4         …KE.67/11580538#Unit Cyanistes caeruleus…  1         60.32754  24.64944
#> 5                …JX.328070#23 Pechipogo strigilat…        NA  61.54883  21.64192
#> 6   …HR.3691/OBS889952298_Unit Parus major Linnaeu…  5         66.6165   24.68816
#> 7               …JX.1025818#50 Garrulus glandarius…  1         63.43302  27.74526
#> 8  …HR.3691/OBS1104249688_Unit Alauda arvensis Lin…        NA  60.05577  24.06798
#> 9          …KE.67/8616657#Unit Cyanistes caeruleus…  1         61.5      24.83333
#> 10                …JX.101375#9 Poecile montanus (C…  7         63.09517  23.1341 
#> ...with 0 more record and 7 more variables:
#> date_time, coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>
