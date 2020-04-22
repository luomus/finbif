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
#> 1  admin_status             
#> 2  red_list                 
#> 3  countries                
#> 4  provinces                
#> 5  municipalities           
#> 6  bird_assoc_areas         
#> 7  finnish_occurrence_status
#> 8  habitat_types            
#> 9  habitat_qualifiers       
#> 10 life_stages              
#> 11 record_basis             
#> 12 restriction_levels       
#> 13 restriction_reasons      
#> 14 sex_categories           
#> 15 sources                  
#> 16 taxon_ranks              

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
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#>   [1] "Algae"                                                     
#>   [2] "Macro algae"                                               
#>   [3] "Brown algae and yellow green algae"                        
#>   [4] "Green algae"                                               
#>   [5] "Red algae"                                                 
#>   [6] "Stoneworts"                                                
#>   [7] "Birds"                                                     
#>   [8] "Crustaceans"                                               
#>   [9] "Macrocrustaceans"                                          
#>  [10] "Amphipods, isopods, opossum shrimps"                       
#>  [11] "Crabs, shrimps and crayfishes"                             
#>  [12] "Other macrocrustaceans"                                    
#>  [13] "Woodlice"                                                  
#>  [14] "Microcrustaceans"                                          
#>  [15] "Branchiopoda"                                              
#>  [16] "Copepods"                                                  
#>  [17] "Seed shrimps"                                              
#>  [18] "Fishes"                                                    
#>  [19] "Fungi and lichens"                                         
#>  [20] "Ascomycota"                                                
#>  [21] "Erysiphales"                                               
#>  [22] "Non-lichenized Ascomycota"                                 
#>  [23] "Lichenized and lichenicolous fungi"                        
#>  [24] "Lichenicolous fungi"                                       
#>  [25] "Lichenized fungi"                                          
#>  [26] "Macrofungi"                                                
#>  [27] "Agaricoid fungi"                                           
#>  [28] "Aphyllophoroid fungi"                                      
#>  [29] "Cantharelloid fungi"                                       
#>  [30] "Clavarioid fungi"                                          
#>  [31] "Corticioid fungi"                                          
#>  [32] "Hydnoid fungi"                                             
#>  [33] "Jelly fungi, tremelloid fungi"                             
#>  [34] "Polypores"                                                 
#>  [35] "Ramarioid fungi"                                           
#>  [36] "Boletoid fungi"                                            
#>  [37] "Cyphelloid fungi"                                          
#>  [38] "Gastroid fungi, puffballs"                                 
#>  [39] "Parasitic microfungi"                                      
#>  [40] "Exobasibium"                                               
#>  [41] "Rust fungi"                                                
#>  [42] "Smut fungi"                                                
#>  [43] "Insects and arachnids"                                     
#>  [44] "Aquatic insects"                                           
#>  [45] "Caddisflies"                                               
#>  [46] "Mayflies"                                                  
#>  [47] "Stoneflies"                                                
#>  [48] "Archaeognatha and Zygentoma"                               
#>  [49] "Beetles"                                                   
#>  [50] "Booklice"                                                  
#>  [51] "Cockroaches"                                               
#>  [52] "Dragonflies"                                               
#>  [53] "Earwigs"                                                   
#>  [54] "Fleas"                                                     
#>  [55] "Harvestmen"                                                
#>  [56] "Katydids, crickets, grasshopper"                           
#>  [57] "Lice"                                                      
#>  [58] "Mites and ticks"                                           
#>  [59] "Moths and butterflies"                                     
#>  [60] "Butterflies"                                               
#>  [61] "Geometer moths"                                            
#>  [62] "Macrolepidoptera"                                          
#>  [63] "Microlepidoptera"                                          
#>  [64] "Owlet moths"                                               
#>  [65] "Sphinx moths"                                              
#>  [66] "Net-winged insects, megalopterans, snakeflies, mecopterans"
#>  [67] "Pseudoscorpions"                                           
#>  [68] "Sawflies, wasps, ants and bees"                            
#>  [69] "Spiders"                                                   
#>  [70] "Thrips"                                                    
#>  [71] "True bugs"                                                 
#>  [72] "Auchenorrhynchans"                                         
#>  [73] "Bladloppor"                                                
#>  [74] "Bladlöss"                                                  
#>  [75] "Heteropterans"                                             
#>  [76] "Soft scales"                                               
#>  [77] "Whiteflies"                                                
#>  [78] "True flies"                                                
#>  [79] "Brachyceran flies"                                         
#>  [80] "Nematoceran flies"                                         
#>  [81] "Twisted-wing parasites"                                    
#>  [82] "Mammals"                                                   
#>  [83] "Bats"                                                      
#>  [84] "Molluscs"                                                  
#>  [85] "Aquatic snails"                                            
#>  [86] "Bivalves"                                                  
#>  [87] "Landsnails and slugs"                                      
#>  [88] "Mosses"                                                    
#>  [89] "Hornworts"                                                 
#>  [90] "Liverworts"                                                
#>  [91] "Mosses"                                                    
#>  [92] "Myriapods"                                                 
#>  [93] "Chilopods, centipedes"                                     
#>  [94] "Millipedes"                                                
#>  [95] "Pauropods"                                                 
#>  [96] "Symphylans"                                                
#>  [97] "Other organisms"                                           
#>  [98] "Bacteria"                                                  
#>  [99] "Entognatha"                                                
#> [100] "Slime molds"                                               
#> [101] "Sponges, bryozoans and cnidarians"                         
#> [102] "Viruses and viroids"                                       
#> [103] "Reptiles and amphibians"                                   
#> [104] "Amphibians"                                                
#> [105] "Reptiles"                                                  
#> [106] "Vascular plants"                                           
#> [107] "Worms"                                                     
#> [108] "Annelids"                                                  
#> [109] "Earthworms, oligochaetes"                                  
#> [110] "Leeches"                                                   
#> [111] "Polychaetes"                                               
#> [112] "Parasitic worms"                                           
#> [113] "Monogeneans"                                               
#> [114] "Tapeworms"                                                 
#> [115] "Priapulid worms"                                           
#> [116] "Ribbon worms"                                              
#> [117] "Turbellarians"                                             
#> attr(,"class")
#> [1] "translation"

```

</details>
<br>

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
#> HR.1227 Coll Mikko Heikkinen              Biota                                  62
#> HR.1349 JYV - Fungal collections          <NA>                                13397
#> HR.1350 JYV - Lichen collections          <NA>                                  398
#> HR.1351 JYV - Bryophyte collections       <NA>                                 3227
#> HR.1467 Per-Eric Grankvist´s butterly co… Lepidoptera                             5
#> HR.1487 JYV - Fish collections            <NA>                                 1371
#> HR.1507 Lingonblad Birger och Hjördis bu… Lepidoptera                          2797
#> HR.1592 Herbarium of The Ark Nature Cent… <NA>                                 1669
#> HR.1687 Papilionoidea of Coll. Lauro      Papilionoidea                         525
#> HR.1688 Noctuidae I of Coll. Lauro        Noctuidae                             614
#> HR.1689 Noctuidae II of Coll. Lauro       Noctuidae                             839
#> HR.1690 Noctuidae III, Bombycoidea, Sphi… Noctuidae, Bombycoidea, Geometri…     521
#> HR.1691 Drepanidae & Geometridae of Coll… Drepanidae, Geometridae              1408
#> HR.175  National Finnish butterfly monit… Lepidoptera                        349862
#> HR.1953 Drainage basin data               <NA>                                    1
#> HR.200  Finnish Insect Database           Insecta                           3729202
#> HR.2049 Invasive alien species control    Invasive species                       93
#> HR.206  The Finnish Nature League's Spri… biota                               86806
#> HR.2089 Håkan Lindberg collection         Hymenoptera                          2295
#> HR.209  Atlas of Finnish Macrolepidoptera Macrolepidoptera                  1217381
#> HR.2209 KUO Arachnida collection          Arachnida                               3
#> HR.2289 Specimens that lack collecting i… <NA>                                  109
#> HR.2691 Line transect censuses of breedi… Aves                               588124
#> HR.2692 Censuses of breeding birds - Are… Aves                                14963
#> HR.3051 Viekas project invasive species … <NA>                                  662
#> HR.3071 Observing species on milk farms   <NA>                                  160
#> HR.3211 iNaturalist                       <NA>                                20463
#> HR.39   Winter Bird Census                Aves                              1365558
#> HR.435  Löydös Open Invasive Species Obs… Biota                               11742
#> HR.60   Monitoring scheme of birds and m… Aves, Mammalia                     771270
#> HR.627  Invasive mammal species of Finla… Mammalia                              228
#> HR.808  E. Sjöholm´s butterfly collection Lepidoptera                          4951
#> HR.847  Atlas of amphibians and reptiles… Amphibia, Reptilia                   5142

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

#>        collection_name abbreviation description online_url has_children is_part_of data_quality
#> HR.129 Collections of… H            Herbarium … <NA>        TRUE        HR.128     MY.dataQual…
#> HR.160 Zoological col… MZH          The collec… http://ww…  TRUE        HR.128     MY.dataQual…
#> HR.173 Zoological mon… <NA>         Monitoring… <NA>        TRUE        HR.128     MY.dataQual…
#> HR.203 Löydös Open Fi… <NA>         A service … http://ws…  TRUE        HR.128     MY.dataQual…
#> HR.447 Hatikka.fi obs… <NA>         Hatikka.fi… http://ha… FALSE        HR.128     MY.dataQual…
#> HR.48  Ringing and re… TIPU         Database o… <NA>        TRUE        HR.128     MY.dataQual…
#>        methods collection_type taxonomic_coverage geographic_coverage temporal_coverage
#> HR.129 NA      MY.collectionT… <NA>               <NA>                <NA>             
#> HR.160 NA      MY.collectionT… Animalia           World               1700 to present  
#> HR.173 NA      MY.collectionT… <NA>               Finland             1950-            
#> HR.203 NA      MY.collectionT… biota              world               2013-            
#> HR.447 NA      MY.collectionT… Biota              World               <NA>             
#> HR.48  NA      MY.collectionT… <NA>               Ringing data: Finl… 1913-            
#>        secure_level count   
#> HR.129 <NA>             1512
#> HR.160 MX.secureLe…      953
#> HR.173 <NA>          4143322
#> HR.203 <NA>            14572
#> HR.447 <NA>          2012066
#> HR.48  <NA>         11944694

```

</details>
<br>
