---
title: "Filtering FinBIF records"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Filtering FinBIF records}
  %\VignetteEngine{knitr::knitr}
  %\VignetteEncoding{UTF-8}
---


When getting records from FinBIF there are many options for filtering the data
before it is downloaded saving bandwidth and local post-processing time. For the
full list of filtering options see `?filters`.

## Loading finbif

```r
library(finbif)
```

## Informal taxonomic groups
You can filter occurrence records based on informal taxonomic groups such as
`Birds` or `Mammals`.

```r
finbif_occurrence(filters = list(informal_group = c("Birds", "Mammals")))
```

```
#> Records downloaded: 10
#> Records available: 17344896
#> A data.frame [10 x 27]
#>         scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1       Chloris chloris         1  61.10167  21.55725 2019-08-06 05:30:00
#> 2   Cyanistes caeruleus         1  61.10167  21.55725 2019-08-06 05:30:00
#> 3     Dryocopus martius         1  60.97243  26.15545 2019-08-05 21:00:00
#> 4  Lophophanes cristat…         1  61.10167  21.55725 2019-08-06 05:30:00
#> 5        Periparus ater         1  61.10167  21.55725 2019-08-06 05:30:00
#> 6  Troglodytes troglod…         1  61.10167  21.55725 2019-08-06 05:30:00
#> 7         Columba oenas         2  60.95741  26.10679 2019-08-05 21:00:00
#> 8           Parus major         3  61.10167  21.55725 2019-08-06 05:30:00
#> 9          Sylvia borin         5  60.97162  26.15558 2019-08-05 21:00:00
#> 10      Curruca curruca         6  60.97162  26.15619 2019-08-05 21:00:00
#> ...with 0 more records and 22 more fields:
#> taxon_rank, country, province, municipality, wkt_wgs84, area_m2,
#> date_start, date_end, hour_start, hour_end, minute_start, minute_end,
#> record_id, event_id, collection_id, is_breeding_site, any_issues,
#> record_reliable, taxon_reliability, document_reliablity,
#> coordinate_accuracy, duration
```

See `finbif_informal_groups()` for the full list of groups you can filter by. 
You can use the same function to see the subgroups that make up a higher
level informal group:

```r
finbif_informal_groups("macrofungi")
```

```
#>  ¦--Macrofungi                                                
#>  ¦   ¦--Agaricoid fungi                                       
#>  ¦   ¦--Aphyllophoroid fungi                                  
#>  ¦   ¦   ¦--Cantharelloid fungi                               
#>  ¦   ¦   ¦--Clavarioid fungi                                  
#>  ¦   ¦   ¦--Corticioid fungi                                  
#>  ¦   ¦   ¦--Hydnoid fungi                                     
#>  ¦   ¦   ¦--Jelly fungi, tremelloid fungi                     
#>  ¦   ¦   ¦--Polypores                                         
#>  ¦   ¦   °--Ramarioid fungi                                   
#>  ¦   ¦--Boletoid fungi                                        
#>  ¦   ¦--Cyphelloid fungi                                      
#>  ¦   °--Gastroid fungi, puffballs
```

## Administrative status
Many records in the FinBIF database include taxa that have one or another
administrative statuses. See `finbif_admin_status()` for a list of
administrative statuses and short-codes.

```r
# Search for birds on the EU invasive species list
finbif_occurrence(
  filters = list(informal_group = "Birds", administrative_status = "EU_INVSV")
)
```

```
#> Records downloaded: 10
#> Records available: 425
#> A data.frame [10 x 24]
#>         scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1  Alopochen aegyptiaca         2  51.90871  4.532580 2018-10-20 09:10:00
#> 2  Alopochen aegyptiaca         2  53.37130  6.143095 2017-10-22 06:00:00
#> 3  Alopochen aegyptiaca         2  53.19242  5.437417 2017-10-24 08:06:00
#> 4  Alopochen aegyptiaca         3  51.74641  4.535283 2018-10-21 10:00:00
#> 5  Alopochen aegyptiaca         4  53.36759  6.191796 2018-10-26 08:15:00
#> 6  Alopochen aegyptiaca         5  53.32081  6.192341 2017-10-23 09:15:00
#> 7  Alopochen aegyptiaca         6  53.37574  6.207861 2018-10-23 05:30:00
#> 8  Alopochen aegyptiaca        20  53.32081  6.192341 2017-10-23 09:15:00
#> 9  Alopochen aegyptiaca        30  52.33990  5.069133 2018-10-22 07:45:00
#> 10 Alopochen aegyptiaca        36  51.74641  4.535283 2018-10-21 10:00:00
#> ...with 0 more records and 19 more fields:
#> taxon_rank, country, municipality, wkt_wgs84, date_start, date_end,
#> hour_start, hour_end, minute_start, minute_end, record_id, event_id,
#> collection_id, any_issues, record_reliable, taxon_reliability,
#> document_reliablity, coordinate_accuracy, duration
```

## IUCN red list
Filtering can be done by [IUCN red list](https://www.iucnredlist.org) 
category. See `finbif_red_list()` for the IUCN red list categories and their 
short-codes.

```r
# Search for near threatened mammals
finbif_occurrence(
  filters = list(informal_group = "Mammals", red_list_status = "NT")
)
```

```
#> Records downloaded: 10
#> Records available: 565
#> A data.frame [10 x 21]
#>         scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1          Castor fiber         1  60.21394  24.71316 2018-10-09 21:00:00
#> 2  Rangifer tarandus f…         1  63.26554  25.36645 2019-06-27 21:00:00
#> 3  Rangifer tarandus f…         1  62.79461  23.07180 2018-08-31 21:00:00
#> 4          Castor fiber         1  61.70314  21.79521 2019-04-20 21:00:00
#> 5  Rangifer tarandus f…         2  63.27123  25.35634 2019-06-27 21:00:00
#> 6  Rangifer tarandus f…         3  63.13691  22.92659 2019-03-05 22:00:00
#> 7  Rangifer tarandus f…         4  63.03293  24.32905 2019-06-12 21:00:00
#> 8  Rangifer tarandus f…         4  60.07458  22.76416 2019-04-10 21:00:00
#> 9  Rangifer tarandus f…         7  63.13690  22.92677 2019-03-06 22:00:00
#> 10 Rangifer tarandus f…        12  64.26828  28.97068 2018-11-12 22:00:00
#> ...with 0 more records and 16 more fields:
#> taxon_rank, country, province, municipality, wkt_wgs84, area_m2,
#> date_start, date_end, record_id, event_id, collection_id, any_issues,
#> record_reliable, taxon_reliability, document_reliablity,
#> coordinate_accuracy
```

## Habitat type
Many taxa are associated with one or more primary or secondary habitat types
(e.g., forest) or subtypes (e.g., herb-rich alpine birch forests). Use
`finbif_habitat_types()` to see the habitat types in FinBIF. You can filter
occurrence records based on primary (or primary/secondary) habitat type or
subtype codes. Note that filtering based on habitat is on taxa not on the
location (i.e., filtering records with `primary_habitat = "M"` will only return
records of taxa considered to primarily inhabit forests, yet the locations of
those records may encompass habitats other than forests).

```r
head(finbif_habitat_types())
```

```
#>                                habitat_type code
#> 1                                   Forests    M
#> 2                             Heath forests   MK
#> 3 Sub-xeric, xeric and barren heath forests  MKK
#> 4         Mesic and herb-rich heath forests  MKT
#> 5 Herb-rich forests (also spruce-dominated)   ML
#> 6           Dry and mesic herb-rich forests  MLT
```

```r
# Search records of taxa for which forests are their primary or secondary
# habitat type
finbif_occurrence(filters = c(primary_secondary_habitat = "M"))
```

```
#> Records downloaded: 10
#> Records available: 18733285
#> A data.frame [10 x 22]
#>         scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1     Apamea monoglypha         1  60.21378  24.71501 2019-08-06 21:00:00
#> 2     Catocala fulminea         1  61.16987  25.55175 2019-08-06 21:00:00
#> 3  Eupithecia inturbata         1  60.21378  24.71501 2019-08-06 21:00:00
#> 4  Geometra papilionar…         1  60.16985  23.59639 2019-08-05 21:00:00
#> 5      Idaea straminata         1  60.14065  24.22098 2019-08-06 21:00:00
#> 6     Oligia latruncula         1  60.21378  24.71501 2019-08-06 21:00:00
#> 7  Parastichtis suspec…         1  60.21378  24.71501 2019-08-06 21:00:00
#> 8  Xanthorhoe ferrugata         1  60.21378  24.71501 2019-08-06 21:00:00
#> 9   Xestia xanthographa         2  60.21378  24.71501 2019-08-06 21:00:00
#> 10    Lymantria monacha         6  60.16985  23.59639 2019-08-05 21:00:00
#> ...with 0 more records and 17 more fields:
#> taxon_rank, country, province, municipality, wkt_wgs84, area_m2,
#> date_start, date_end, record_id, event_id, collection_id, life_stage,
#> any_issues, record_reliable, taxon_reliability, document_reliablity,
#> coordinate_accuracy
```

You may further refine habitat based searching using a specific habitat type
qualifier such as "sun-exposed" or "shady". Use `finbif_habitat_qualifiers()`
to see the qualifiers available. To specify qualifiers use a named list of
character vectors where the names are habitat types or subtypes and the elements
of the character vectors are the qualifier codes.

```r
finbif_habitat_qualifiers()[4:6, ]
```

```
#>                          habitat_type code
#> 4 Broadleaved deciduous trees present    J
#> 5                         Sun-exposed   PA
#> 6                               Shady   VA
```

```r
# Search records of taxa for which forests with sun-exposure and broadleaved
# deciduous trees are their primary habitat type
finbif_occurrence(filters = list(primary_habitat = list(M = c("PA", "J"))))
```

```
#> Records downloaded: 10
#> Records available: 103
#> A data.frame [10 x 24]
#>     scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1  Pammene fasciana         1  59.96020  20.99521 2017-09-19 21:00:00
#> 2  Pammene fasciana         1  60.00217  23.43591 2019-06-21 21:00:00
#> 3  Pammene fasciana         1  60.36807  26.17233 2017-06-28 21:00:00
#> 4  Pammene fasciana         1  60.35244  19.83238 2017-07-29 21:00:00
#> 5  Pammene fasciana         1  60.35244  19.83238 2017-07-30 21:00:00
#> 6  Pammene fasciana         1  60.35244  19.83238 2017-08-01 21:00:00
#> 7  Pammene fasciana         1  60.35244  19.83238 2017-08-02 21:00:00
#> 8  Pammene fasciana         1  60.21166  24.90204 2017-07-30 21:00:00
#> 9  Pammene fasciana         2  60.35244  19.83238 2017-08-02 21:00:00
#> 10 Pammene fasciana         3  60.00217  23.43591 2019-05-07 21:00:00
#> ...with 0 more records and 19 more fields:
#> taxon_rank, country, province, municipality, wkt_wgs84, area_m2,
#> date_start, date_end, record_id, event_id, collection_id,
#> female_abundance, male_abundance, life_stage, any_issues,
#> record_reliable, taxon_reliability, document_reliablity,
#> coordinate_accuracy
```
