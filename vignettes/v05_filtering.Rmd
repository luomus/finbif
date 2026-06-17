---
title: "Filtering occurrence records"
author: "William K. Morris"
output: 
  rmarkdown::html_vignette:
    toc: true
vignette: >
  %\VignetteIndexEntry{5. Filtering occurrence records}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---


When getting records from FinBIF there are many options for filtering the data
before it is downloaded, saving bandwidth and local post-processing time. For
the full list of filtering options see `?filters`.

## Location
Records can be filtered by the name of a location.

``` r
finbif_occurrence(filter = c(country = "Finland"))
#> Records downloaded: 10
#> Records available: 57360604
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

Or by a set of coordinates.

``` r
finbif_occurrence(
  filter = list(coordinates = list(c(60, 68), c(20, 30), "wgs84"))
)
#> Records downloaded: 10
#> Records available: 48564194
#> A data.frame [10 x 12]
#>    occurrenceID       scientificName individualCount decimalLatitude decimalLongitude
#> 1           …21 Polytrichum juniper…              NA  60.17967        24.914629      
#> 2           …25 Polytrichum juniper…              NA  60.373472       24.993816      
#> 3           …29 Polytrichum juniper…              NA  61.612783       21.44191       
#> 4           …33 Polytrichum juniper…              NA  61.322069       23.513515      
#> 5           …37 Polytrichum juniper…              NA  61.249458       25.040691      
#> 6           …41 Polytrichum juniper…              NA  62.605448       25.925676      
#> 7           …49 Polytrichum juniper…              NA  66.004079       28.202282      
#> 8           …57 Polytrichum pilifer…              NA  60.373472       24.993816      
#> 9           …61 Polytrichum pilifer…              NA  61.599004       21.434943      
#> 10          …65 Polytrichum pilifer…              NA  61.452593       24.099408      
#> ...with 0 more records and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus
```

See `?filters` section "Location" for more details

## Time
The event or import date of records can be used to filter occurrence data from
FinBIF. The date filters can be a single year, month or date,

``` r
finbif_occurrence(filter = list(date_range_ym = "2020-12"))
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 30157
#> A data.frame [10 x 12]
#>                 occurrenceID       scientificName individualCount decimalLatitude
#> 1  …herb.oulu.fi/MY.10184972 Orthotrichum anomal…              NA  61.603872     
#> 2  …herb.oulu.fi/MY.10185111 Schistidium submuti…              NA  61.603833     
#> 3  …herb.oulu.fi/MY.10313974 Skeletocutis bigutt…              NA  60.234691     
#> 4  …herb.oulu.fi/MY.10314039 Oxyporus populinus …              NA  61.467278     
#> 5  …herb.oulu.fi/MY.10314043 Stereum hirsutum (W…              NA  61.467278     
#> 6  …herb.oulu.fi/MY.10314116 Stereum sanguinolen…              NA  60.201527     
#> 7  …herb.oulu.fi/MY.10745815 Eurhynchium angusti…              NA  61.467746     
#> 8     …luomus.fi/MY.10204437 Dendrocopos leucoto…              NA  61.6077       
#> 9     …luomus.fi/MY.10221155 Allophylaria macros…              NA  60.378251     
#> 10    …luomus.fi/MY.10221158 Host: Chamaenerion …              NA  60.378251     
#> ...with 0 more records and 8 more variables:
#> decimalLongitude, eventDateTime, coordinateUncertaintyInMeters, hasIssues,
#> requiresVerification, requiresIdentification, occurrenceReliability,
#> identificationVerificationStatus

```

</details>
<br>
, or for record events, a range as a character vector.

``` r
finbif_occurrence(
  filter = list(date_range_ymd = c("2019-06-01", "2019-12-31"))
)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 1167813
#> A data.frame [10 x 12]
#>    occurrenceID       scientificName individualCount decimalLatitude decimalLongitude
#> 1          …244 Aneura pinguis (L.)…              NA  61.86848        24.042062      
#> 2          …248 Sphenolobus saxicol…              NA  61.790156       24.739934      
#> 3          …264 Barbilophozia hatch…              NA  62.172979       23.166974      
#> 4          …268 Barbilophozia hatch…              NA  61.733229       23.557042      
#> 5          …276 Barbilophozia hatch…              NA  62.341331       23.821755      
#> 6          …280 Blepharostoma trich…              NA  61.817804       23.156312      
#> 7          …288 Calypogeia integris…              NA  61.787466       24.740131      
#> 8          …296 Calypogeia muelleri…              NA  61.787466       24.740131      
#> 9          …317 Cephalozia bicuspid…              NA  61.768742       23.877258      
#> 10         …388 Lophocolea heteroph…              NA  61.4436         24.119939      
#> ...with 0 more records and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus

```

</details>
<br>

Records for a specific season or time-span across all years can also be
requested.

``` r
finbif_occurrence(
  filter = list(
    date_range_md = c(begin = "12-21", end = "12-31"),
    date_range_md = c(begin = "01-01", end = "02-20")
  )
)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 1793259
#> A data.frame [10 x 12]
#>    occurrenceID       scientificName individualCount decimalLatitude decimalLongitude
#> 1       …079640 Pohlia nutans (Hedw…              NA  60.321276       24.109857      
#> 2       …184972 Orthotrichum anomal…              NA  61.603872       24.227601      
#> 3       …185111 Schistidium submuti…              NA  61.603833       24.225719      
#> 4       …225336 Fuscocephaloziopsis…              NA  61.510093       24.343093      
#> 5       …225392 Lophocolea minor Ne…              NA  61.65722        24.658173      
#> 6       …225400 Obtusifolium obtusu…              NA  61.458209       23.658786      
#> 7       …225403 Plagiomnium cuspida…              NA  61.458209       23.658786      
#> 8       …281093 Pseudanomodon atten…              NA  61.374279       24.197437      
#> 9       …281097 Pseudanomodon atten…              NA  61.571317       24.305839      
#> 10      …281100 Syntrichia ruralis …              NA  61.571317       24.305839      
#> ...with 0 more records and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus

```

</details>
<br>

## Data Quality
You can filter occurrence records by indicators of data quality. See `?filters`
section "Quality" for details.

``` r
strict <- c(
  collection_quality = "professional", coordinates_uncertainty_max = 1,
  record_quality = "expert_verified"
)
permissive <- list(
  wild_status = c("wild", "non_wild", "wild_unknown"),
  record_quality = c(
    "expert_verified", "community_verified", "unassessed", "uncertain",
    "erroneous"
  ),
  abundance_min = 0
)
c(
  strict     = finbif_occurrence(filter = strict,     count_only = TRUE),
  permissive = finbif_occurrence(filter = permissive, count_only = TRUE)
)
#>     strict permissive 
#>      82631   61265240
```

## Collection
The FinBIF database consists of a number of constituent collections. You can
filter by collection with either the `collection` or `not_collection` filters.
Use `finbif_collections()` to see metadata on the FinBIF collections.

``` r
finbif_occurrence(
  filter = c(collection = "iNaturalist Suomi Finland"), count_only = TRUE
)
#> [1] 1673340
finbif_occurrence(
  filter = c(collection = "Notebook, general observations"), count_only = TRUE
)
#> [1] 3546019
```

## Informal taxonomic groups
You can filter occurrence records based on informal taxonomic groups such as
`Birds` or `Mammals`.

``` r
finbif_occurrence(filter = list(informal_groups = c("Birds", "Mammals")))
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 29626954
#> A data.frame [10 x 12]
#>                 occurrenceID       scientificName individualCount decimalLatitude
#> 1  …herb.oulu.fi/MY.17469610 Asio flammeus (Pont…              NA  67.718422     
#> 2  …herb.oulu.fi/MY.17483993 Picoides tridactylu…              NA  67.47766      
#> 3     …luomus.fi/MY.10042206 Strix uralensis Pal…              NA  63.811181     
#> 4     …luomus.fi/MY.10042210 Strix uralensis Pal…              NA  60.360916     
#> 5     …luomus.fi/MY.10042213 Bubo bubo (Linnaeus…              NA  60.930307     
#> 6     …luomus.fi/MY.10042218 Bubo bubo (Linnaeus…              NA  63.681831     
#> 7     …luomus.fi/MY.10042223 Bubo bubo (Linnaeus…              NA  60.135148     
#> 8     …luomus.fi/MY.10042226 Strix uralensis Pal…              NA  62.250643     
#> 9     …luomus.fi/MY.10042231 Astur gentilis (Lin…              NA  63.811181     
#> 10    …luomus.fi/MY.10042238 Accipiter nisus (Li…              NA  60.2522       
#> ...with 0 more records and 8 more variables:
#> decimalLongitude, eventDateTime, coordinateUncertaintyInMeters, hasIssues,
#> requiresVerification, requiresIdentification, occurrenceReliability,
#> identificationVerificationStatus

```

</details>
<br>

See `finbif_informal_groups()` for the full list of groups you can filter by.
You can use the same function to see the subgroups that make up the highest
level informal groups:

``` r
finbif_informal_groups("Birds")
#> Birds
#>   --Birds of prey and owls
#>       --Owls
#>       --Birds of prey
#>   --Waterbirds
```

## Regulatory 
Many records in the FinBIF database include taxa that have one or another
regulatory statuses. See `finbif_metadata("regulatory_status")` for a list of
regulatory statuses and short-codes.

``` r
# Search for birds on the EU invasive species list
finbif_occurrence(
  filter = list(informal_groups = "Birds", regulatory_status = "EU_INVSV")
)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 507
#> A data.frame [10 x 12]
#>    occurrenceID       scientificName individualCount decimalLatitude decimalLongitude
#> 1      …7350700 Pycnonotus cafer su…  3                           NA               NA
#> 2      …7351780 Corvus splendens su…  2                           NA               NA
#> 3      …7351784 Corvus splendens su…  2                           NA               NA
#> 4      …7351788 Corvus splendens su…  2                           NA               NA
#> 5      …7351792 Corvus splendens su…  1                           NA               NA
#> 6      …7352178 Oxyura jamaicensis …  7                           NA               NA
#> 7      …7352182 Oxyura jamaicensis …  8                           NA               NA
#> 8      …7355894 Oxyura jamaicensis …  8                           NA               NA
#> 9      …7430682 Corvus splendens su…  3                           NA               NA
#> 10     …8449767 Alopochen aegyptiac…              NA              NA               NA
#> ...with 0 more records and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus

```

</details>
<br>

## IUCN red list
Filtering can be done by [IUCN red list](https://punainenkirja.laji.fi/)
category. See `finbif_metadata("red_list")` for the IUCN red list categories and
their short-codes.

``` r
# Search for near threatened mammals
finbif_occurrence(
  filter = list(informal_groups = "Mammals", red_list_status = "NT")
)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 82019
#> A data.frame [10 x 12]
#>    occurrenceID       scientificName individualCount decimalLatitude decimalLongitude
#> 1       …223201 Microtus arvalis (P…              NA              NA               NA
#> 2       …223245 Microtus arvalis (P…              NA              NA               NA
#> 3       …223249 Microtus arvalis (P…              NA              NA               NA
#> 4       …223253 Microtus arvalis (P…              NA              NA               NA
#> 5       …223565 Microtus arvalis (P…              NA              NA               NA
#> 6       …223659 Castor fiber Linnae…              NA              NA               NA
#> 7       …223878 Castor fiber Linnae…              NA              NA               NA
#> 8       …580236 Castor fiber Linnae…              NA              NA               NA
#> 9       …580240 Castor fiber Linnae…              NA              NA               NA
#> 10      …580398 Ursus arctos Linnae…              NA  67.05           29.25          
#> ...with 0 more records and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus

```

</details>
<br>

## Habitat type
Many taxa are associated with one or more primary or secondary habitat types
(e.g., forest) or subtypes (e.g., herb-rich alpine birch forests). Use
`finbif_metadata("habitat_type")` to see the habitat types in FinBIF. You can
filter occurrence records based on primary (or primary/secondary) habitat type
or subtype codes. Note that filtering based on habitat is on taxa not on the
location (i.e., filtering records with `primary_habitat = "M"` will only return
records of taxa considered to primarily inhabit forests, yet the locations of
those records may encompass habitats other than forests).

``` r
head(finbif_metadata("habitat_type"))
#>                code name                                         
#> MKV.habitatMt  Mt   alpine birch forests                         
#> MKV.habitatTlk Tlk  alpine calcareous rock outcrops and boulder …
#> MKV.habitatTlr Tlr  alpine gorges and canyons                    
#> MKV.habitatT   T    Alpine habitats                              
#> MKV.habitatTp  Tp   alpine heath scrubs                          
#> MKV.habitatTk  Tk   alpine heaths
```

``` r
# Search records of taxa for which forests are their primary or secondary
# habitat type
finbif_occurrence(filter = c(primary_secondary_habitat = "M"))
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 34008277
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

You may further refine habitat based searching using a specific habitat type
qualifier such as "sun-exposed" or "shady". Use
`finbif_metadata("habitat_qualifier")` to see the qualifiers available. To
specify qualifiers use a named list of character vectors where the names are
habitat types or subtypes and the elements of the character vectors are the
qualifier codes.

``` r
finbif_metadata("habitat_qualifier")[4:6, ]
#>                           code name                                 
#> MKV.habitatSpecificTypeCA CA   calcareous effect                    
#> MKV.habitatSpecificTypeH  H    esker forests, also semi-open forests
#> MKV.habitatSpecificTypeLK LK   fishless ponds
```

``` r
# Search records of taxa for which forests with sun-exposure and broadleaved
# deciduous trees are their primary habitat type
finbif_occurrence(filter = list(primary_habitat = list(M = c("PAK", "J"))))
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 218
#> A data.frame [10 x 12]
#>                  occurrenceID       scientificName individualCount decimalLatitude
#> 1   …id.luomus.fi/MY.19077695 Pammene fasciana (L…  1               60.188362     
#> 2  …tun.fi/HR.3211/53817755-U Pammene fasciana (L…              NA  59.90452      
#> 3       …tun.fi/JX.1011605#97 Pammene fasciana (L…  1               60.50396      
#> 4       …tun.fi/JX.1011998#37 Pammene fasciana (L…  1               59.960224     
#> 5      …tun.fi/JX.1012832#367 Pammene fasciana (L…  1               60.002166     
#> 6      …tun.fi/JX.1038248#475 Pammene fasciana (L…              NA  59.934164     
#> 7      …tun.fi/JX.1098381#487 Pammene fasciana (L…              NA  60.045579     
#> 8       …tun.fi/JX.1103286#13 Pammene fasciana (L…  1               59.90522      
#> 9        …tun.fi/JX.1134471#4 Pammene fasciana (L…  2               61.549842     
#> 10     …tun.fi/JX.1143718#265 Pammene fasciana (L…              NA  60.37543      
#> ...with 0 more records and 8 more variables:
#> decimalLongitude, eventDateTime, coordinateUncertaintyInMeters, hasIssues,
#> requiresVerification, requiresIdentification, occurrenceReliability,
#> identificationVerificationStatus

```

</details>
<br>

## Status of taxa in Finland
You can restrict the occurrence records by the status of the taxa in Finland.
For example you can request records for only rare species.

``` r
finbif_occurrence(filter = c(finnish_occurrence_status = "rare"))
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 499022
#> A data.frame [10 x 12]
#>    occurrenceID       scientificName individualCount decimalLatitude decimalLongitude
#> 1      …3990009 Amanita coryli Nevi…              NA  68.058832       24.058368      
#> 2      …3990121 Amanita flavescens …              NA  64.756355       26.197092      
#> 3      …3993454 Amanita flavescens …  2               66.65863        27.482198      
#> 4      …3993458 Amanita coryli Nevi…  3               66.371875       27.409632      
#> 5      …4018238 Hydnum jussii Niska…              NA  64.368844       28.004874      
#> 6      …4018310 Lamelloclavaria pet…              NA  64.421905       27.677572      
#> 7      …4018314 Lamelloclavaria pet…              NA  64.323694       28.044601      
#> 8      …8016027 Amanita coryli Nevi…  5               66.168299       25.765382      
#> 9      …8016087 Amanita coryli Nevi…  3               69.007453       20.930394      
#> 10     …8016091 Amanita coryli Nevi…  2               68.185593       23.992918      
#> ...with 0 more records and 7 more variables:
#> eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,
#> requiresIdentification, occurrenceReliability, identificationVerificationStatus

```

</details>
<br>
Or, by using the negation of occurrence status, you can request records of birds
excluding those considered vagrants.

``` r
finbif_occurrence(
  filter = list(
    informal_groups               = "birds",
    finnish_occurrence_status_neg = sprintf("vagrant_%sregular", c("", "ir"))
  )
)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 29048611
#> A data.frame [10 x 12]
#>                 occurrenceID       scientificName individualCount decimalLatitude
#> 1  …herb.oulu.fi/MY.17469610 Asio flammeus (Pont…              NA  67.718422     
#> 2  …herb.oulu.fi/MY.17483993 Picoides tridactylu…              NA  67.47766      
#> 3     …luomus.fi/MY.10042206 Strix uralensis Pal…              NA  63.811181     
#> 4     …luomus.fi/MY.10042210 Strix uralensis Pal…              NA  60.360916     
#> 5     …luomus.fi/MY.10042213 Bubo bubo (Linnaeus…              NA  60.930307     
#> 6     …luomus.fi/MY.10042218 Bubo bubo (Linnaeus…              NA  63.681831     
#> 7     …luomus.fi/MY.10042223 Bubo bubo (Linnaeus…              NA  60.135148     
#> 8     …luomus.fi/MY.10042226 Strix uralensis Pal…              NA  62.250643     
#> 9     …luomus.fi/MY.10042231 Astur gentilis (Lin…              NA  63.811181     
#> 10    …luomus.fi/MY.10042238 Accipiter nisus (Li…              NA  60.2522       
#> ...with 0 more records and 8 more variables:
#> decimalLongitude, eventDateTime, coordinateUncertaintyInMeters, hasIssues,
#> requiresVerification, requiresIdentification, occurrenceReliability,
#> identificationVerificationStatus

```

</details>
<br>

See `finbif_metadata("finnish_occurrence_status")` for a full list of statuses
and their descriptions.
