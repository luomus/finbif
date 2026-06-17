# Selecting and ordering variables

When requesting data using [finbif](https://github.com/luomus/finbif)
you can select from among (or order by) many variables (properties of
the occurrence records). The default set of variables returned is a
small subset of those available. Note that not all variables are
available for all records. See
[`?variables`](https://finbif-docs-dev.netlify.app/reference/variables.md)
for details.

## Selecting variables

### Limiting variables

To retrieve a limited set of variables from FinBIF simply specify the
desired variables in the `select` argument.

``` r

finbif_occurrence(
  genus  = "Falco",
  select = c("scientificName", "lifeStage", "sex")
)
```

Click to show/hide output.

``` r


#> Records downloaded: 10
#> Records available: 473049
#> A data.frame [10 x 3]
#>          scientificName lifeStage    sex
#> 1  Falco rusticolus Li…      <NA> Female
#> 2  Falco peregrinus Tu…      <NA>   <NA>
#> 3  Falco subbuteo Linn…      <NA>   <NA>
#> 4  Falco columbarius L…      <NA>   <NA>
#> 5  Falco columbarius L…      <NA>   <NA>
#> 6  Falco tinnunculus L…      <NA>   <NA>
#> 7  Falco columbarius L…      <NA> Female
#> 8  Falco tinnunculus L…      <NA> Female
#> 9  Falco tinnunculus L…      <NA> Female
#> 10 Falco vespertinus L…  juvenile   <NA>
```

  

### Extra variables

To get extra variables as well as the default set, specify the extra
variables in addition to the keyword `"default_vars"`.

``` r

finbif_occurrence(select = c("default_vars", "lifeStage"))
```

Click to show/hide output.

``` r


#> Records downloaded: 10
#> Records available: 60179991
#> A data.frame [10 x 13]
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
#> ...with 0 more record and 8 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus, lifeStage
```

  

## Ordering

You can change the order of occurrence records before they are fetched
from the server by using the `order_by` argument. The default ordering
is `date_start` descending, then `load_date` descending, then
`reported_name`.

### Ascending order

By default occurrence records are ordered by variables in ascending
order.

``` r

finbif_occurrence("Cygnus cygnus", order_by = "individualCount")
```

Click to show/hide output.

``` r


#> Records downloaded: 10
#> Records available: 139829
#> A data.frame [10 x 12]
#>    occurrenceID       scientificName individualCount decimalLatitude decimalLongitude
#> 1      …0745928 Cygnus cygnus (Linn…              NA  66              29.25          
#> 2      …0745932 Cygnus cygnus (Linn…              NA              NA               NA
#> 3      …0745937 Cygnus cygnus (Linn…              NA              NA               NA
#> 4      …0745941 Cygnus cygnus (Linn…              NA  60.17           25             
#> 5      …0745945 Cygnus cygnus (Linn…              NA  60.17           25             
#> 6      …0745949 Cygnus cygnus (Linn…              NA  60.17           25             
#> 7      …5552466 Cygnus cygnus (Linn…  1               67.773235       25.050461      
#> 8      …5552474 Cygnus cygnus (Linn…  1               68.713901       22.787894      
#> 9      …6082839 Cygnus cygnus (Linn…  1                           NA               NA
#> 10     …7079647 Cygnus cygnus (Linn…              NA  61.85           23.5           
#> ...with 0 more record and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus
```

  

### Descending order

You can switch to descending order by prefixing the variable with a
dash.

``` r

finbif_occurrence("Cygnus cygnus", order_by = "-individualCount")
```

Click to show/hide output.

``` r


#> Records downloaded: 10
#> Records available: 139829
#> A data.frame [10 x 12]
#>                   occurrenceID       scientificName individualCount decimalLatitude
#> 1                 …MHU.2981587 Cygnus cygnus (Linn…  6000            64.4          
#> 2  …HR.3691/OBS2052501157_Unit Cygnus cygnus (Linn…  2065            64.50736      
#> 3  …HR.3691/OBS2052518008_Unit Cygnus cygnus (Linn…  2065            64.50736      
#> 4  …HR.3691/OBS1101526155_Unit Cygnus cygnus (Linn…  1760            62.16389      
#> 5   …HR.3691/OBS604642304_Unit Cygnus cygnus (Linn…  1753            64.50736      
#> 6   …HR.3691/OBS663568887_Unit Cygnus cygnus (Linn…  1600            65.98787      
#> 7  …HR.3691/OBS1399623409_Unit Cygnus cygnus (Linn…  1580            64.50736      
#> 8                …MHU.28815250 Cygnus cygnus (Linn…  1500                        NA
#> 9   …HR.3691/OBS671353848_Unit Cygnus cygnus (Linn…  1361            64.71656      
#> 10 …HR.3691/OBS1686954463_Unit Cygnus cygnus (Linn…  1333            64.50734      
#>    decimalLongitude
#> 1  -14.54          
#> 2   24.27894       
#> 3   24.27894       
#> 4   21.45786       
#> 5   24.27894       
#> 6   24.06341       
#> 7   24.27894       
#> 8                NA
#> 9   24.531883      
#> 10  24.278942      
#> ...with 0 more record and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus
```

  

### Multiple variables

You can specify multiple variables to order by. Sorting primacy is from
left to right.

``` r

finbif_occurrence(
  "Cygnus olor", order_by = c("finnishCountyID", "-individualCount")
)
```

Click to show/hide output.

``` r


#> Records downloaded: 10
#> Records available: 63244
#> A data.frame [10 x 12]
#>    occurrenceID       scientificName individualCount decimalLatitude decimalLongitude
#> 1     …12015253 Cygnus olor (J.F. G…  2500                        NA               NA
#> 2     …13633876 Cygnus olor (J.F. G…  1500                        NA               NA
#> 3       …780984 Cygnus olor (J.F. G…  1300                        NA               NA
#> 4       …781416 Cygnus olor (J.F. G…  1300                        NA               NA
#> 5      …2062563 Cygnus olor (J.F. G…  1200                        NA               NA
#> 6     …10352894 Cygnus olor (J.F. G…  1100                        NA               NA
#> 7       …927797 Cygnus olor (J.F. G…  1050                        NA               NA
#> 8      …3110953 Cygnus olor (J.F. G…  960                         NA               NA
#> 9     …26309317 Cygnus olor (J.F. G…  900                         NA               NA
#> 10     …1260385 Cygnus olor (J.F. G…  800             58.66           23.57          
#> ...with 0 more record and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus
```

  

### Random sample

You can also request a random sample (random order) of occurrence
records by setting the `sample` argument to `TRUE`.

``` r

finbif_occurrence(sample = TRUE)
```

Click to show/hide output.

``` r


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

  
