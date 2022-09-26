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

#>    metadata_name            
#> 1  regulatory_status        
#> 2  red_list                 
#> 3  country                  
#> 4  region                   
#> 5  bio_province             
#> 6  municipality             
#> 7  bird_assoc_area          
#> 8  finnish_occurrence_status
#> 9  habitat_type             
#> 10 habitat_qualifier        
#> 11 life_stage               
#> 12 record_basis             
#> 13 restriction_level        
#> 14 restriction_reason       
#> 15 sex_category             
#> 16 source                   
#> 17 taxon_rank               

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

#>    status_name           status_code
#> 1  Critically Endangered CR         
#> 2  Data Deficient        DD         
#> 3  Endangered            EN         
#> 4  Extinct               EX         
#> 5  Extinct in the Wild   EW         
#> 6  Least Concern         LC         
#> 7  Near Threatened       NT         
#> 8  Not Applicable        NA         
#> 9  Not Evaluated         NE         
#> 10 Regionally Extinct    RE         
#> 11 Vulnerable            VU         

```

</details>
<br>

## Special cases
Some more complex metadata is accessed with other `{finbif}` functions

### Informal groups
Informal taxonomic groups and their relationships can be displayed with
`finbif_informal_groups()`

```r
finbif_informal_groups(limit = 6)
#> Algae                                                         
#>  °--Macro algae                                               
#>      ¦--Brown algae and yellow green algae                    
#>      ¦--Green algae                                           
#>      ¦--Red algae                                             
#>      °--Stoneworts                                            
#> ...142 more groups
```

You can select a subgroup by specifying a parent informal group as a function
argument.

```r
finbif_informal_groups("Crustaceans")
#> Crustaceans                                                   
#>  ¦--Macrocrustaceans                                          
#>  ¦   ¦--Amphipods, isopods, opossum shrimps                   
#>  ¦   ¦--Crabs, shrimps and crayfishes                         
#>  ¦   ¦--Other macrocrustaceans                                
#>  ¦   °--Woodlice                                              
#>  °--Microcrustaceans                                          
#>      ¦--Branchiopoda                                          
#>      ¦--Copepods                                              
#>      °--Seed shrimps
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
#> HR.1227 Coll Mikko Heikkinen              Biota                                  67
#> HR.1349 JYV - Fungal collections          <NA>                                13399
#> HR.1350 JYV - Lichen collections          <NA>                                  398
#> HR.1351 JYV - Bryophyte collections       <NA>                                 4369
#> HR.1467 Per-Eric Grankvist´s butterly co… Lepidoptera                             5
#> HR.1487 JYV - Fish collections            <NA>                                 1371
#> HR.1507 Lingonblad Birger och Hjördis bu… Lepidoptera                          2796
#> HR.157  Point counts of breeding terrest… Birds, landbirds                   376833
#> HR.1592 Herbarium of The Ark Nature Cent… <NA>                                 7806
#> HR.1687 Papilionoidea of Coll. Lauro      Papilionoidea                         525
#> HR.1688 Noctuidae I of Coll. Lauro        Noctuidae                             614
#> HR.1689 Noctuidae II of Coll. Lauro       Noctuidae                             839
#> HR.1690 Noctuidae III, Bombycoidea, Sphi… Noctuidae, Bombycoidea, Geometri…     521
#> HR.1691 Drepanidae & Geometridae of Coll… Drepanidae, Geometridae              1408
#> HR.175  National Finnish butterfly monit… Lepidoptera                        408864
#> HR.200  Finnish Insect Database           Insecta                           3725818
#> HR.2049 Invasive alien species control    Invasive species                      471
#> HR.206  The Finnish Nature League's Spri… biota                              107275
#> HR.2089 Håkan Lindberg collection         Hymenoptera                          2400
#> HR.209  Atlas of Finnish Macrolepidoptera Macrolepidoptera                  1218544
#> HR.2209 KUO Arachnida collection          Arachnida                               3
#> HR.2289 Specimens that lack collecting i… <NA>                                  109
#> HR.2691 Line transect censuses of breedi… Aves                               603950
#> HR.2692 Censuses of breeding birds - Are… Aves                                14963
#> HR.3051 VieKas LIFE project invasive spe… <NA>                                 1349
#> HR.3071 Observing species on milk farms   <NA>                                  529
#> HR.3211 iNaturalist Suomi Finland         biota                              417052
#> HR.3491 LajiGIS: Aquatic species survey   Biota                              518824
#> HR.3553 LajiGIS: Species monitoring sites Biota                              657410
#> HR.3671 Bird of prey nests for protection Aves                                10246
#> HR.3691 eBird                             Aves                               528002
#> HR.3791 Invasive species observations     Biota                                1385
#> HR.39   Winter Bird Census                Aves                              1404097
#> HR.3911 Bumblebee census                  Bumblebees                           4154
#> HR.3991 Waterbird counts, Luomus dataset  Aves                                 2424
#> HR.3992 Waterbird counts, Luke dataset    Aves                                15012
#> HR.4011 Salmonidae in streams             Salmonidae                          12630
#> HR.4051 LajiGIS: Species monitoring site… Aquila chrysaetos; Haliaeetus al…    7780
#> HR.4091 Retkikasvio                       <NA>                                 1227
#> HR.4131 Butterflies in Finnish agricultu… Papilionoidea, Others              356560
#> HR.435  Löydös Open Invasive Species Obs… Biota                               17157
#> HR.60   Monitoring scheme of birds and m… Aves, Mammalia                     826341
#> HR.627  Invasive mammal species of Finla… Mammalia                              228
#> HR.808  E. Sjöholm´s butterfly collection Lepidoptera                          4951
#> HR.847  Atlas of amphibians and reptiles… Amphibia, Reptilia                   6233

```

</details>
<br>

By default, `finbif_collections()` only displays the lowest level collections.
Higher level, "supercollections" can be viewed by setting
`supercollections = TRUE` and you can limit the output to collections with
a minimum number of records in them with the `nmin` argument.

```r
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

You can see the child collections of a supercollection by specifying the ID as
a filter. Note that the children of supercollections may also be supercollections

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
#> HR.203  Löydös Open Fi… <NA>         A service … http://ws…  TRUE        HR.128     MY.dataQual…
#> HR.447  Hatikka.fi obs… <NA>         Hatikka.fi… http://ha… FALSE        HR.128     MY.dataQual…
#> HR.48   Ringing and re… TIPU         Database o… <NA>        TRUE        HR.128     MY.dataQual…
#>         methods   collection_type taxonomic_coverage geographic_coverage temporal_coverage
#> HR.129  <NA>      MY.collectionT… <NA>               <NA>                <NA>             
#> HR.160  <NA>      MY.collectionT… Animalia           World               1700 to present  
#> HR.173  <NA>      MY.collectionT… <NA>               Finland             1950-            
#> HR.1849 Sampling… MY.collectionT… Biota              World               2000-            
#> HR.203  <NA>      MY.collectionT… biota              world               2013-            
#> HR.447  <NA>      MY.collectionT… Biota              World               <NA>             
#> HR.48   <NA>      MY.collectionT… <NA>               Ringing data: Finl… 1913-            
#>         secure_level count   
#> HR.129  <NA>             1513
#> HR.160  MX.secureLe…      940
#> HR.173  <NA>          4765423
#> HR.1849 <NA>                1
#> HR.203  <NA>            26650
#> HR.447  <NA>          2010389
#> HR.48   <NA>         12456603

```

</details>
<br>
