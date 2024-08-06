---
title: "Metadata"
author: "William K. Morris"
output: 
  rmarkdown::html_vignette:
    toc: true
vignette: >
  %\VignetteIndexEntry{4. Metadata}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---


Much of the information in the FinBIF database consists of metadata that helps
provide context for occurrence records and other information in FinBIF.

## General metadata
You can see some of the metadata available in `{finbif}` by calling the
`finbif_metadata` function without any arguments.

```r
finbif_metadata()
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#>                metadata_name
#> 1          regulatory_status
#> 2                   red_list
#> 3                    country
#> 4                     region
#> 5               bio_province
#> 6               municipality
#> 7            bird_assoc_area
#> 8  finnish_occurrence_status
#> 9               habitat_type
#> 10         habitat_qualifier
#> 11                life_stage
#> 12              record_basis
#> 13         restriction_level
#> 14        restriction_reason
#> 15              sex_category
#> 16                    source
#> 17                taxon_rank

```

</details>
<br>

Calling `finbif_metadata()` and specifying one of the metadata categories will
display a `data.frame` with the requested metadata.

```r
finbif_metadata("red_list")
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#>           code name                 
#> MX.iucnCR CR   Critically Endangered
#> MX.iucnDD DD   Data Deficient       
#> MX.iucnEN EN   Endangered           
#> MX.iucnEX EX   Extinct              
#> MX.iucnEW EW   Extinct in the Wild  
#> MX.iucnLC LC   Least Concern        
#> MX.iucnNT NT   Near Threatened      
#> MX.iucnNA NA   Not Applicable       
#> MX.iucnNE NE   Not Evaluated        
#> MX.iucnRE RE   Regionally Extinct   
#> MX.iucnVU VU   Vulnerable           

```

</details>
<br>

## Special cases
Some more complex metadata is accessed with other `{finbif}` functions

### Informal groups
Informal taxonomic groups and their relationships can be displayed with
`finbif_informal_groups()`

```r
finbif_informal_groups(limit = 2)
#> Algae
#>   --Macro algae
#>       --Brown algae and yellow green algae
#>       --Green algae
#>       --Red algae
#>       --Stoneworts
#> Birds
#>   --Birds of prey and owls
#>       --Birds of prey
#>       --Owls
#>   --Waterbirds
#> ...137 more groups
```

You can select a subgroup by specifying a parent informal group as a function
argument.

```r
finbif_informal_groups("Crustaceans")
#> Crustaceans
#>   --Macrocrustaceans
#>       --Amphipods, isopods, opossum shrimps
#>       --Crabs, shrimps and crayfishes
#>       --Other macrocrustaceans
#>       --Woodlice
#>   --Microcrustaceans
#>       --Branchiopoda
#>       --Copepods
#>       --Seed shrimps
```

## Collections
Another special case of metadata is `finbif_collections()`. Collections are the
highest level of record aggregation in the FinBIF database.

You can subset collection metadata by using the `filter` and `select` arguments.

```r
finbif_collections(
  filter = geographic_coverage == "Finland",
  select = c("collection_name", "taxonomic_coverage", "count")
)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#>         collection_name                   taxonomic_coverage                count  
#> HR.1227 Priv. coll. Mikko Heikkinen       Biota                                  62
#> HR.1349 JYV - Fungal collections          <NA>                                13477
#> HR.1350 JYV - Lichen collections          <NA>                                  608
#> HR.1351 JYV - Bryophyte collections       <NA>                                 8367
#> HR.1467 Per-Eric Grankvist´s butterly co… Lepidoptera                             5
#> HR.1487 JYV - Fish collections            <NA>                                 1371
#> HR.1507 Lingonblad Birger och Hjördis bu… Lepidoptera                          2796
#> HR.157  Point counts of breeding terrest… Birds, landbirds                   393361
#> HR.1592 Herbarium of The Ark Nature Cent… <NA>                                 7871
#> HR.1687 Papilionoidea of Coll. Lauro      Papilionoidea                         550
#> HR.1688 Noctuidae I of Coll. Lauro        Noctuidae                             614
#> HR.1689 Noctuidae II of Coll. Lauro       Noctuidae                             839
#> HR.1690 Noctuidae III, Bombycoidea, Sphi… Noctuidae, Bombycoidea, Geometri…     521
#> HR.1691 Drepanidae & Geometridae of Coll… Drepanidae, Geometridae              1408
#> HR.175  National Finnish butterfly monit… Lepidoptera                        450525
#> HR.1916 Wildlife triangle                 Siberian flying squirrel (Pterom…   18560
#> HR.200  Finnish Insect Database           Insecta                           3725381
#> HR.2009 Fish observation data from the n… invasive alien fish species - mu…   35582
#> HR.2049 Invasive alien species control    Invasive species                     1502
#> HR.206  The Finnish Nature League's Spri… biota                              119323
#> HR.2089 Håkan Lindberg collection         Hymenoptera                          2435
#> HR.209  Atlas of Finnish Macrolepidoptera Macrolepidoptera                  1218555
#> HR.2129 Fungal atlas                      fungi                              116519
#> HR.2209 KUO Arachnida collection          Arachnida                               3
#> HR.2289 Specimens that lack collecting i… <NA>                                  109
#> HR.2691 Luomus line transect censuses of… Aves                               628987
#> HR.2692 Censuses of breeding birds - Are… Aves                                14963
#> HR.3051 VieKas LIFE project invasive spe… <NA>                                 1545
#> HR.3071 Observing species on milk farms   <NA>                                  529
#> HR.3211 iNaturalist Suomi Finland         biota                              691050
#> HR.3491 LajiGIS: Aquatic species survey   Biota                              643837
#> HR.3553 LajiGIS: Species monitoring sites Biota                              731965
#> HR.3671 Bird of prey nests for protection Aves                                13323
#> HR.3691 eBird                             Aves                              1100308
#> HR.3791 Invasive species observations     Biota                                3268
#> HR.39   Winter Bird Census                Aves, Mammalia                    1483842
#> HR.3911 Bumblebee census                  Bumblebees                          29949
#> HR.3991 Waterbird counts, Luomus dataset  Aves                                39236
#> HR.3992 Waterbird counts, Luke dataset 2… Aves                                30145
#> HR.4011 Salmonidae in streams             Salmonidae                          12630
#> HR.4051 LajiGIS: Species monitoring site… Aquila chrysaetos; Haliaeetus al…    8669
#> HR.4091 Retkikasvio                       <NA>                                   84
#> HR.4131 Butterflies in Finnish agricultu… Papilionoidea, Others              356987
#> HR.4191 Porvoo Museum / Butterfly Collec… Lepidoptera                         10417
#> HR.4251 LajiGIS: Species mapping and sur… Biota                              490816
#> HR.435  Löydös Open Invasive Species Obs… Biota                               19232
#> HR.4352 NFI rare tree species             <NA>                                  995
#> HR.4412 Tiira.fi: The Fourth Breeding Bi… Aves                               329445
#> HR.4471 4th Finnish Bird Atlas 2022–2025… Aves                               165551
#> HR.4511 Finnish National Moth Monitoring  Bombycoidea, Noctuoidea, Sphingi… 1156373
#> HR.4611 Observations by FCG Finnish Cons… <NA>                                  462
#> HR.4612 Pollinator monitoring, line tran… Insecta                              7885
#> HR.4672 Observations from publications    Biota                                 127
#> HR.4711 Flying squirrel monitoring        Pteromys volans                      2509
#> HR.4991 Bird atlas observations manually… Aves                                 6620
#> HR.5095 Dragonfly complete lists          Odonata                                29
#> HR.5155 Observations from electronic sou… Biota                                1372
#> HR.5196 Pollinator monitoring, pan traps  Insecta                              5688
#> HR.5235 Finnish butterflies - complete l… Lepidoptera, Papilinoidea              21
#> HR.5236 Charismatic flowering plants - c… Tracheophyta                          277
#> HR.60   Monitoring scheme of birds and m… Aves, Mammalia                     874130
#> HR.627  Alien mammal species observation… Invasive alien mammal species – …    3094
#> HR.808  E. Sjöholm´s butterfly collection Lepidoptera                          4951
#> HR.847  Atlas of amphibians and reptiles… Amphibia, Reptilia                   6690

```

</details>
<br>

By default, `finbif_collections()` only displays the lowest level collections.
Higher level, "supercollections" can be viewed by setting
`supercollections = TRUE` and you can limit the output to collections with
a minimum number of records in them with the `nmin` argument.

``` r
collections <- finbif_collections(supercollections = TRUE, nmin = 10000)
```

The `finbif_collections()` function returns a `data.frame` where the row names
are the ID number of the collection.

```r
finbif_collections(supercollections = TRUE)["HR.128", "collection_name"]
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#> Collections of the Finnish Museum of Natural History Luomus

```

</details>
<br>

You can see the child collections of a supercollection by specifying the ID as a
filter. Note that the children of supercollections may also be
supercollections.

```r
finbif_collections(is_part_of == "HR.128", supercollections = TRUE)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#>         collection_name abbreviation description online_url has_children is_part_of data_quality
#> HR.129  Collections of… H            Herbarium … <NA>        TRUE        HR.128     MY.dataQual…
#> HR.160  Zoological col… MZH          The collec… http://ww…  TRUE        HR.128     MY.dataQual…
#> HR.173  Zoological mon… <NA>         Monitoring… <NA>        TRUE        HR.128     MY.dataQual…
#> HR.1849 Genomic resour… <NA>         Genomic re… <NA>        TRUE        HR.128     MY.dataQual…
#> HR.203  Löydös Open Fi… <NA>         A service … https://l… FALSE        HR.128     <NA>        
#> HR.435  Löydös Open In… <NA>         A service … https://l… FALSE        HR.128     MY.dataQual…
#> HR.447  Hatikka.fi obs… <NA>         Hatikka.fi… http://ha… FALSE        HR.128     MY.dataQual…
#> HR.48   Ringing and re… TIPU         Dataset co… <NA>        TRUE        HR.128     MY.dataQual…
#>         methods   collection_type taxonomic_coverage geographic_coverage temporal_coverage
#> HR.129  <NA>      MY.collectionT… <NA>               <NA>                <NA>             
#> HR.160  <NA>      MY.collectionT… Animalia           World               1700 to present  
#> HR.173  <NA>      MY.collectionT… <NA>               Finland             1950-            
#> HR.1849 Sampling… MY.collectionT… Biota              World               2000-            
#> HR.203  <NA>      MY.collectionT… biota              world               2013-            
#> HR.435  <NA>      MY.collectionT… Biota              Finland             2015-            
#> HR.447  <NA>      MY.collectionT… Biota              World               <NA>             
#> HR.48   <NA>      MY.collectionT… Aves               Ringing data: Finl… 1913-            
#>         secure_level count   
#> HR.129  <NA>                2
#> HR.160  MX.secureLe…      742
#> HR.173  <NA>          5160221
#> HR.1849 <NA>                1
#> HR.203  <NA>            39375
#> HR.435  <NA>            19232
#> HR.447  <NA>          2010032
#> HR.48   <NA>         12910437

```

</details>
<br>
