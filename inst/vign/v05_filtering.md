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

```r
finbif_occurrence(filter = c(country = "Finland"))
#> Records downloaded: 10
#> Records available: 44691386
#> A data.frame [10 x 12]
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
#> ...with 0 more record and 7 more variables:
#> date_time, coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality
```

Or by a set of coordinates.

```r
finbif_occurrence(
  filter = list(coordinates = list(c(60, 68), c(20, 30), "wgs84"))
)
#> Records downloaded: 10
#> Records available: 37318868
#> A data.frame [10 x 12]
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
#> ...with 0 more record and 7 more variables:
#> date_time, coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality
```

See `?filters` section "Location" for more details

## Time
The event or import date of records can be used to filter occurrence data from
FinBIF. The date filters can be a single year, month or date,

```r
finbif_occurrence(filter = list(date_range_ym = c("2019-12")))
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 20150
#> A data.frame [10 x 12]
#>                     record_id      scientific_name abundance lat_wgs84 lon_wgs84
#> 1  …KE.921/LGE.627772/1470480 Pteromys volans (Li…        NA  61.81362  25.75756
#> 2             …JX.1054648#107 Pica pica (Linnaeus…  3         65.30543  25.70355
#> 3              …JX.1054648#85 Poecile montanus (C…  1         65.30543  25.70355
#> 4             …JX.1054648#103 Garrulus glandarius…  3         65.30543  25.70355
#> 5             …JX.1054648#123 Passer montanus (Li…  3         65.30543  25.70355
#> 6             …JX.1054648#149 Pyrrhula pyrrhula (…  1         65.30543  25.70355
#> 7              …JX.1054648#93 Cyanistes caeruleus…  9         65.30543  25.70355
#> 8              …JX.1054648#95 Parus major Linnaeu…  35        65.30543  25.70355
#> 9             …JX.1054648#137 Carduelis flammea (…  2         65.30543  25.70355
#> 10            …JX.1056695#107 Pica pica (Linnaeus…  6         62.7154   23.0893 
#>              date_time
#> 1  2019-12-31 12:00:00
#> 2  2019-12-31 10:20:00
#> 3  2019-12-31 10:20:00
#> 4  2019-12-31 10:20:00
#> 5  2019-12-31 10:20:00
#> 6  2019-12-31 10:20:00
#> 7  2019-12-31 10:20:00
#> 8  2019-12-31 10:20:00
#> 9  2019-12-31 10:20:00
#> 10 2019-12-31 10:15:00
#> ...with 0 more record and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>
, or for record events, a range as a character vector.

```r
finbif_occurrence(
  filter = list(date_range_ymd = c("2019-06-01", "2019-12-31"))
)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 911735
#> A data.frame [10 x 12]
#>                     record_id      scientific_name abundance lat_wgs84 lon_wgs84
#> 1  …KE.921/LGE.627772/1470480 Pteromys volans (Li…        NA  61.81362  25.75756
#> 2             …JX.1054648#107 Pica pica (Linnaeus…  3         65.30543  25.70355
#> 3              …JX.1054648#85 Poecile montanus (C…  1         65.30543  25.70355
#> 4             …JX.1054648#103 Garrulus glandarius…  3         65.30543  25.70355
#> 5             …JX.1054648#123 Passer montanus (Li…  3         65.30543  25.70355
#> 6             …JX.1054648#149 Pyrrhula pyrrhula (…  1         65.30543  25.70355
#> 7              …JX.1054648#93 Cyanistes caeruleus…  9         65.30543  25.70355
#> 8              …JX.1054648#95 Parus major Linnaeu…  35        65.30543  25.70355
#> 9             …JX.1054648#137 Carduelis flammea (…  2         65.30543  25.70355
#> 10            …JX.1056695#107 Pica pica (Linnaeus…  6         62.7154   23.0893 
#>              date_time
#> 1  2019-12-31 12:00:00
#> 2  2019-12-31 10:20:00
#> 3  2019-12-31 10:20:00
#> 4  2019-12-31 10:20:00
#> 5  2019-12-31 10:20:00
#> 6  2019-12-31 10:20:00
#> 7  2019-12-31 10:20:00
#> 8  2019-12-31 10:20:00
#> 9  2019-12-31 10:20:00
#> 10 2019-12-31 10:15:00
#> ...with 0 more record and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

Records for a specific season or time-span across all years can also be
requested.

```r
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
#> Records available: 1486845
#> A data.frame [10 x 12]
#>      record_id      scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1  …433443#318 Accipiter nisus (Li…  1         64.8162   25.32106 2023-02-20 15:00:00
#> 2  …531663#107 Pica pica (Linnaeus…  10        62.9199   27.71032 2023-02-20 07:40:00
#> 3  …530610#107 Pica pica (Linnaeus…  21        65.78623  24.49119 2023-02-20 09:15:00
#> 4  …530449#107 Pica pica (Linnaeus…  4         65.74652  24.62216 2023-02-20 08:20:00
#> 5  …531663#153 Emberiza citrinella…  12        62.9199   27.71032 2023-02-20 07:40:00
#> 6   …531663#49 Columba livia domes…  10        62.9199   27.71032 2023-02-20 07:40:00
#> 7   …530610#49 Columba livia domes…  2         65.78623  24.49119 2023-02-20 09:15:00
#> 8  …530610#117 Corvus corax Linnae…  1         65.78623  24.49119 2023-02-20 09:15:00
#> 9   …531663#61 Dendrocopos major (…  6         62.9199   27.71032 2023-02-20 07:40:00
#> 10 …531663#111 Corvus monedula Lin…  7         62.9199   27.71032 2023-02-20 07:40:00
#> ...with 0 more record and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

## Data Quality
You can filter occurrence records by indicators of data quality. See `?filters`
section "Quality" for details.

```r
strict <- c(
  collection_quality = "professional", coordinates_uncertainty_max = 1,
  record_quality = "expert_verified"
)
permissive <- list(
  quality_issues = "both",
  record_reliability = c("reliable", "unassessed", "unreliable"),
  record_quality = c(
    "expert_verified", "community_verified", "unassessed", "uncertain",
    "erroneous"
  )
)
c(
  strict     = finbif_occurrence(filter = strict,     count_only = TRUE),
  permissive = finbif_occurrence(filter = permissive, count_only = TRUE)
)
#> Error: 1 error occurred:
#>   - Invalid name in record reliability: unassessed
```

## Collection
The FinBIF database consists of a number of constituent collections. You can
filter by collection with either the `collection` or `not_collection` filters.
Use `finbif_collections()` to see metadata on the FinBIF collections.

```r
finbif_occurrence(
  filter = c(collection = "iNaturalist Suomi Finland"), count_only = TRUE
)
#> [1] 691076
finbif_occurrence(
  filter = c(collection = "Notebook, general observations"), count_only = TRUE
)
#> [1] 2110409
```

## Informal taxonomic groups
You can filter occurrence records based on informal taxonomic groups such as
`Birds` or `Mammals`.

```r
finbif_occurrence(filter = list(informal_groups = c("Birds", "Mammals")))
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 22116048
#> A data.frame [10 x 12]
#>    record_id      scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1       …5#3 Sciurus vulgaris Li…  1         60.23584  25.05693 2023-06-14 08:56:00
#> 2       …2#9 Hirundo rustica Lin…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 3      …2#37 Pica pica (Linnaeus…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 4      …2#49 Muscicapa striata (…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 5      …2#39 Larus canus Linnaeu…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 6       …2#5 Emberiza citrinella…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 7      …2#31 Ficedula hypoleuca …        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 8      …2#41 Alauda arvensis Lin…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 9      …2#21 Numenius arquata (L…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 10     …2#29 Dendrocopos major (…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> ...with 0 more record and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

See `finbif_informal_groups()` for the full list of groups you can filter by.
You can use the same function to see the subgroups that make up a higher
level informal group:

```r
finbif_informal_groups("macrofungi")
#> Error in finbif_informal_groups("macrofungi"): Group not found
```

## Regulatory 
Many records in the FinBIF database include taxa that have one or another
regulatory statuses. See `finbif_metadata("regulatory_status")` for a list of
regulatory statuses and short-codes.

```r
# Search for birds on the EU invasive species list
finbif_occurrence(
  filter = list(informal_groups = "Birds", regulatory_status = "EU_INVSV")
)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 471
#> A data.frame [10 x 12]
#>                                 record_id      scientific_name abundance lat_wgs84 lon_wgs84
#> 1                           …JX.1580858#3 Oxyura jamaicensis …  1         60.28687  25.0271 
#> 2                           …JX.1580860#3 Oxyura jamaicensis …  1         60.28671  25.02713
#> 3  …KE.176/62b1ad90d5deb0fafdc6212b#Unit1 Oxyura jamaicensis …  7         61.66207  23.57706
#> 4                          …JX.1045316#34 Alopochen aegyptiac…  3         52.16081  4.485534
#> 5                          …JX.138840#123 Alopochen aegyptiac…  4         53.36759  6.191796
#> 6                          …JX.139978#214 Alopochen aegyptiac…  6         53.37574  6.207861
#> 7                           …JX.139710#17 Alopochen aegyptiac…  30        52.3399   5.069133
#> 8                           …JX.139645#57 Alopochen aegyptiac…  36        51.74641  4.535283
#> 9                           …JX.139645#10 Alopochen aegyptiac…  3         51.74641  4.535283
#> 10                          …JX.139442#16 Alopochen aegyptiac…  2         51.90871  4.53258 
#> ...with 0 more record and 7 more variables:
#> date_time, coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

## IUCN red list
Filtering can be done by [IUCN red list](https://punainenkirja.laji.fi/)
category. See `finbif_metadata("red_list")` for the IUCN red list categories and
their short-codes.

```r
# Search for near threatened mammals
finbif_occurrence(
  filter = list(informal_groups = "Mammals", red_list_status = "NT")
)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 42510
#> A data.frame [10 x 12]
#>                                 record_id      scientific_name abundance lat_wgs84 lon_wgs84
#> 1                          …JX.1594024#23 Rangifer tarandus f…  15        63.31266  24.43298
#> 2                        …JX.1588853#1075 Rangifer tarandus f…  1         63.84551  29.8366 
#> 3                           …JX.1593780#3 Pusa hispida botnic…  1         65.02313  25.40505
#> 4                    …HR.3211/166639315-U Rangifer tarandus f…        NA  63.7      24.7    
#> 5                    …HR.3211/166049302-U Rangifer tarandus f…        NA  64.1      26.5    
#> 6                    …HR.3211/165761924-U Rangifer tarandus f…        NA  63.9      24.9    
#> 7                         …JX.1589779#105 Rangifer tarandus f…  3         63.7261   23.40827
#> 8  …KE.176/647ad84dd5de884fa20e25e6#Unit1 Rangifer tarandus f…  1         64.12869  24.73877
#> 9                    …HR.3211/165005253-U Pusa hispida botnic…        NA  64.2865   23.87402
#> 10                         …JX.1588052#18 Rangifer tarandus f…  2         64.13286  26.26767
#> ...with 0 more record and 7 more variables:
#> date_time, coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

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

```r
head(finbif_metadata("habitat_type"))
#>                code name                                              
#> MKV.habitatMt  Mt   alpine birch forests (excluding herb-rich alpine …
#> MKV.habitatTlk Tlk  alpine calcareous rock outcrops and boulder fields
#> MKV.habitatTlr Tlr  alpine gorges and canyons                         
#> MKV.habitatT   T    Alpine habitats                                   
#> MKV.habitatTp  Tp   alpine heath scrubs                               
#> MKV.habitatTk  Tk   alpine heaths
```

```r
# Search records of taxa for which forests are their primary or secondary
# habitat type
finbif_occurrence(filter = c(primary_secondary_habitat = "M"))
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 26362337
#> A data.frame [10 x 12]
#>    record_id      scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1       …5#3 Sciurus vulgaris Li…  1         60.23584  25.05693 2023-06-14 08:56:00
#> 2      …2#37 Pica pica (Linnaeus…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 3      …2#49 Muscicapa striata (…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 4       …2#5 Emberiza citrinella…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 5      …2#31 Ficedula hypoleuca …        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 6      …2#29 Dendrocopos major (…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 7      …2#15 Sylvia borin (Bodda…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 8      …2#11 Anthus trivialis (L…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 9      …2#45 Corvus monedula Lin…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 10      …2#3 Phylloscopus trochi…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> ...with 0 more record and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

You may further refine habitat based searching using a specific habitat type
qualifier such as "sun-exposed" or "shady". Use
`finbif_metadata("habitat_qualifier")` to see the qualifiers available. To
specify qualifiers use a named list of character vectors where the names are
habitat types or subtypes and the elements of the character vectors are the
qualifier codes.

```r
finbif_metadata("habitat_qualifier")[4:6, ]
#>                           code name                                              
#> MKV.habitatSpecificTypeCA CA   calcareous effect                                 
#> MKV.habitatSpecificTypeH  H    esker forests, also semi-open forests             
#> MKV.habitatSpecificTypeKE KE   intermediate-basic rock outcrops and boulder fiel…
```

```r
# Search records of taxa for which forests with sun-exposure and broadleaved
# deciduous trees are their primary habitat type
finbif_occurrence(filter = list(primary_habitat = list(M = c("PAK", "J"))))
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 178
#> A data.frame [10 x 12]
#>      record_id      scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1  …502812#393 Pammene fasciana (L…        NA  60.45845  22.17811 2022-08-14 12:00:00
#> 2    …435062#6 Pammene fasciana (L…  1         60.20642  24.66127          2022-08-04
#> 3    …435050#9 Pammene fasciana (L…  1         60.20642  24.66127          2022-07-25
#> 4   …501598#39 Pammene fasciana (L…  1         60.08841  22.48629 2022-07-21 12:00:00
#> 5  …501387#162 Pammene fasciana (L…  1         60.08841  22.48629 2022-07-20 12:00:00
#> 6  …448030#159 Pammene fasciana (L…  1         60.08841  22.48629 2022-07-18 12:00:00
#> 7   …447556#78 Pammene fasciana (L…  1         60.08841  22.48629 2022-07-14 12:00:00
#> 8  …446841#408 Pammene fasciana (L…  1         60.08841  22.48629 2022-07-12 12:00:00
#> 9   …443339#36 Pammene fasciana (L…  1         60.08841  22.48629 2022-07-10 12:00:00
#> 10 …440849#159 Pammene fasciana (L…  2         60.08841  22.48629 2022-07-08 12:00:00
#> ...with 0 more record and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

## Status of taxa in Finland
You can restrict the occurrence records by the status of the taxa in Finland.
For example you can request records for only rare species.

```r
finbif_occurrence(filter = c(finnish_occurrence_status = "rare"))
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Records downloaded: 10
#> Records available: 406005
#> A data.frame [10 x 12]
#>                                 record_id      scientific_name abundance lat_wgs84 lon_wgs84
#> 1                    …HR.3211/167313706-U Pygaera timon (Hübn…        NA  62.1281   27.45272
#> 2                          …JX.1594282#21 Carterocephalus pal…  1         64.65322  24.58941
#> 3                    …HR.3211/167197097-U Carterocephalus pal…        NA  65.07819  25.55236
#> 4                    …HR.3211/167183358-U Glaucopsyche alexis…        NA  60.46226  22.76647
#> 5                           …JX.1594291#3 Glaucopsyche alexis…  1         60.42692  22.20411
#> 6  …KE.176/6488c111d5de884fa20e295f#Unit1 Panemeria tenebrata…  1         61.16924  25.56036
#> 7                           …JX.1593930#3 Hemaris tityus (Lin…  1         60.63969  27.29052
#> 8  …KE.176/64889455d5de884fa20e294f#Unit1 Pseudopanthera macu…  2         62.054    30.352  
#> 9                         …JX.1594170#199 Glaucopsyche alexis…  1         61.10098  28.68453
#> 10                          …JX.1594112#3 Hemaris tityus (Lin…  1         61.25511  28.89127
#> ...with 0 more record and 7 more variables:
#> date_time, coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>
Or, by using the negation of occurrence status, you can request records of birds
excluding those considered vagrants.

```r
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
#> Records available: 21725426
#> A data.frame [10 x 12]
#>    record_id      scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1         …9 Hirundo rustica Lin…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 2        …37 Pica pica (Linnaeus…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 3        …49 Muscicapa striata (…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 4        …39 Larus canus Linnaeu…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 5         …5 Emberiza citrinella…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 6        …31 Ficedula hypoleuca …        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 7        …41 Alauda arvensis Lin…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 8        …21 Numenius arquata (L…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 9        …29 Dendrocopos major (…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> 10       …15 Sylvia borin (Bodda…        NA  64.12716  23.99111 2023-06-14 08:48:00
#> ...with 0 more record and 6 more variables:
#> coordinates_uncertainty, any_issues, requires_verification, requires_identification,
#> record_reliability, record_quality

```

</details>
<br>

See `finbif_metadata("finnish_occurrence_status")` for a full list of statuses
and their descriptions.
