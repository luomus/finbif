---
title: "Filtering FinBIF records"
author: "William K Morris"
output: 
  rmarkdown::html_vignette:
    toc: true
vignette: >
  %\VignetteIndexEntry{2. Filtering FinBIF records}
  %\VignetteEngine{knitr::rmarkdown}
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
finbif_occurrence(filter = list(informal_group = c("Birds", "Mammals")))
#> Records downloaded: 10
#> Records available: 17370561
#> A data.frame [10 x 30]
#>        scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1  Capreolus capreolus         1  60.82651  21.34383 2019-08-21 02:30:00
#> 2      Lepus europaeus         1  60.80257  21.38201 2019-08-21 02:30:00
#> 3     Sciurus vulgaris         1  60.79812  21.35765 2019-08-21 02:30:00
#> 4        Vulpes vulpes         1  60.82976  21.26560 2019-08-21 02:30:00
#> 5        Lepus timidus         1  60.81778  21.33762 2019-08-21 02:30:00
#> 6        Vulpes vulpes         1  60.42812  22.23448 2019-08-20 21:00:00
#> 7   Erithacus rubecula         1  60.42820  22.24195 2019-08-20 21:00:00
#> 8        Vulpes vulpes         1  60.10824  23.52158 2019-08-20 21:00:00
#> 9     Sciurus vulgaris         1  60.43755  22.24938 2019-08-21 21:00:00
#> 10     Hirundo rustica        24  63.80152  22.89729 2019-08-20 21:00:00
#> ...with 0 more records and 25 more variables:
#> taxon_rank, country, province, municipality, date_start, date_end,
#> hour_start, hour_end, minute_start, minute_end, record_id,
#> individual_id, event_id, collection_id, any_issues, record_issue,
#> record_reliable, taxon_reliability, document_issue,
#> document_reliablity, coordinate_accuracy, event_issue,
#> location_issue, time_issue, duration
```

See `finbif_informal_groups()` for the full list of groups you can filter by. 
You can use the same function to see the subgroups that make up a higher
level informal group:

```r
finbif_informal_groups("macrofungi")
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
  filter = list(informal_group = "Birds", administrative_status = "EU_INVSV")
)
#> Records downloaded: 10
#> Records available: 425
#> A data.frame [10 x 30]
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
#> ...with 0 more records and 25 more variables:
#> taxon_rank, country, province, municipality, date_start, date_end,
#> hour_start, hour_end, minute_start, minute_end, record_id,
#> individual_id, event_id, collection_id, any_issues, record_issue,
#> record_reliable, taxon_reliability, document_issue,
#> document_reliablity, coordinate_accuracy, event_issue,
#> location_issue, time_issue, duration
```

## IUCN red list
Filtering can be done by [IUCN red list](https://www.iucnredlist.org) 
category. See `finbif_red_list()` for the IUCN red list categories and their 
short-codes.

```r
# Search for near threatened mammals
finbif_occurrence(
  filter = list(informal_group = "Mammals", red_list_status = "NT")
)
#> Records downloaded: 10
#> Records available: 568
#> A data.frame [10 x 30]
#>         scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1  Rangifer tarandus f…         1  63.26554  25.36645 2019-06-27 21:00:00
#> 2  Rangifer tarandus f…         1  64.32293  26.69975 2019-05-26 21:00:00
#> 3          Ursus arctos         1  62.31940  24.58560 2018-12-31 22:00:00
#> 4          Ursus arctos         1  62.31940  24.58560 2018-12-31 22:00:00
#> 5          Castor fiber         1  61.70314  21.79521 2019-04-20 21:00:00
#> 6  Rangifer tarandus f…         2  63.27123  25.35634 2019-06-27 21:00:00
#> 7  Rangifer tarandus f…         3  63.13691  22.92659 2019-03-05 22:00:00
#> 8  Rangifer tarandus f…         4  63.03293  24.32905 2019-06-12 21:00:00
#> 9  Rangifer tarandus f…         4  60.07458  22.76416 2019-04-10 21:00:00
#> 10 Rangifer tarandus f…         7  63.13690  22.92677 2019-03-06 22:00:00
#> ...with 0 more records and 25 more variables:
#> taxon_rank, country, province, municipality, date_start, date_end,
#> hour_start, hour_end, minute_start, minute_end, record_id,
#> individual_id, event_id, collection_id, any_issues, record_issue,
#> record_reliable, taxon_reliability, document_issue,
#> document_reliablity, coordinate_accuracy, event_issue,
#> location_issue, time_issue, duration
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
#>                                habitat_type code
#> 1                                   Forests    M
#> 2                             Heath forests   MK
#> 3 Sub-xeric, xeric and barren heath forests  MKK
#> 4         Mesic and herb-rich heath forests  MKT
#> 5 Herb-rich forests (also spruce-dominated)   ML
#> 6           Dry and mesic herb-rich forests  MLT

# Search records of taxa for which forests are their primary or secondary
# habitat type
finbif_occurrence(filter = c(primary_secondary_habitat = "M"))
#> Records downloaded: 10
#> Records available: 18766779
#> A data.frame [10 x 30]
#>         scientific_name abundance lat_wgs84 lon_wgs84           date_time
#> 1  Eudonia truncicolel…         1  62.92172  27.63335 2019-08-21 21:00:00
#> 2  Batrachedra praeang…         1  62.92172  27.63335 2019-08-21 21:00:00
#> 3      Acleris holmiana         1  62.92172  27.63335 2019-08-21 21:00:00
#> 4    Dysstroma citratum         1  62.92172  27.63335 2019-08-21 21:00:00
#> 5     Eulithis populata         1  62.92172  27.63335 2019-08-21 21:00:00
#> 6        Diarsia dahlii         1  62.92172  27.63335 2019-08-21 21:00:00
#> 7      Apamea lateritia         1  62.92172  27.63335 2019-08-21 21:00:00
#> 8  Argyresthia goedart…         1  62.92172  27.63335 2019-08-21 21:00:00
#> 9      Sciurus vulgaris         1  60.43755  22.24938 2019-08-21 21:00:00
#> 10 Caryocolum pullatel…         4  62.92172  27.63335 2019-08-21 21:00:00
#> ...with 0 more records and 25 more variables:
#> taxon_rank, country, province, municipality, date_start, date_end,
#> hour_start, hour_end, minute_start, minute_end, record_id,
#> individual_id, event_id, collection_id, any_issues, record_issue,
#> record_reliable, taxon_reliability, document_issue,
#> document_reliablity, coordinate_accuracy, event_issue,
#> location_issue, time_issue, duration
```

You may further refine habitat based searching using a specific habitat type
qualifier such as "sun-exposed" or "shady". Use `finbif_habitat_qualifiers()`
to see the qualifiers available. To specify qualifiers use a named list of
character vectors where the names are habitat types or subtypes and the elements
of the character vectors are the qualifier codes.

```r
finbif_habitat_qualifiers()[4:6, ]
#>                          habitat_type code
#> 4 Broadleaved deciduous trees present    J
#> 5                         Sun-exposed   PA
#> 6                               Shady   VA

# Search records of taxa for which forests with sun-exposure and broadleaved
# deciduous trees are their primary habitat type
finbif_occurrence(filter = list(primary_habitat = list(M = c("PA", "J"))))
#> Records downloaded: 10
#> Records available: 103
#> A data.frame [10 x 30]
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
#> ...with 0 more records and 25 more variables:
#> taxon_rank, country, province, municipality, date_start, date_end,
#> hour_start, hour_end, minute_start, minute_end, record_id,
#> individual_id, event_id, collection_id, any_issues, record_issue,
#> record_reliable, taxon_reliability, document_issue,
#> document_reliablity, coordinate_accuracy, event_issue,
#> location_issue, time_issue, duration
```
