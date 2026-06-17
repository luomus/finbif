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

``` r
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
#> 6       finnish_municipality
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

``` r
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

``` r
finbif_informal_groups(limit = 2)
#> Birds
#>   --Birds of prey and owls
#>       --Owls
#>       --Birds of prey
#>   --Waterbirds
#> Mammals
#>   --Small mammals
#>   --Bats
#> ...150 more groups
```

You can select a subgroup by specifying a parent informal group as a function
argument.

``` r
finbif_informal_groups("Crustaceans")
#> Crustaceans
#>   --Macrocrustaceans
#>       --Crabs, shrimps and crayfishes
#>       --Amphipods, isopods, opossum shrimps
#>       --Woodlice
#>       --Other macrocrustaceans
#>   --Microcrustaceans
#>       --Copepods
#>       --Branchiopoda
#>       --Seed shrimps
```

## Collections
Another special case of metadata is `finbif_collections()`. Collections are the
highest level of record aggregation in the FinBIF database.

You can subset collection metadata by using the `filter` and `select` arguments.

``` r
finbif_collections(
  filter = geographic_coverage == "Finland",
  select = c(
    "collection_name", "taxonomic_coverage", "geographic_coverage", "count"
  )
)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#>         collection_name        taxonomic_coverage     geographic_coverage count  
#> HR.1227 Priv. coll. Mikko Hei… Biota                  Finland                  48
#> HR.1349 JYV - Fungal collecti… <NA>                   Finland               14486
#> HR.1350 JYV - Lichen collecti… <NA>                   Finland                 825
#> HR.1351 JYV - Bryophyte colle… <NA>                   Finland               11214
#> HR.1467 Per-Eric Grankvist´s … Lepidoptera            Finland                   5
#> HR.1487 JYV - Fish collections <NA>                   Finland                1371
#> HR.1507 Lingonblad Birger och… Lepidoptera            Finland                2799
#> HR.157  Point counts of breed… Birds, landbirds       Finland              425914
#> HR.1592 Main herbarium of The… <NA>                   Finland                7832
#> HR.1687 Papilionoidea of Coll… Papilionoidea          Finland                 550
#> HR.1688 Noctuidae I of Coll. … Noctuidae              Finland                 614
#> HR.1689 Noctuidae II of Coll.… Noctuidae              Finland                 839
#> HR.1690 Noctuidae III, Bombyc… Noctuidae, Bombycoide… Finland                 521
#> HR.1691 Drepanidae & Geometri… Drepanidae, Geometrid… Finland                1408
#> HR.175  National Butterfly Re… Lepidoptera            Finland              549592
#> HR.1916 Wildlife triangle      Siberian flying squir… Finland               18560
#> HR.200  Finnish Insect Databa… Insecta                Finland             3725313
#> HR.2009 Fish observation data… invasive alien fish s… Finland               35854
#> HR.2029 LajiGIS: Miscellaneou… Biota                  Finland               44432
#> HR.2049 Invasive alien specie… Invasive species       Finland                3908
#> HR.206  The Finnish Nature As… biota                  Finland              141107
#> HR.2089 Håkan Lindberg collec… Hymenoptera            Finland                2435
#> HR.209  Atlas of Finnish Macr… Macrolepidoptera       Finland             1218546
#> HR.2129 The Atlas of Finnish … fungi                  Finland              181432
#> HR.2209 KUO Arachnida collect… Arachnida              Finland                   3
#> HR.2289 Specimens that lack c… <NA>                   Finland                 109
#> HR.2691 Luomus line transect … Aves                   Finland              580918
#> HR.2692 Censuses of breeding … Aves                   Finland               14996
#> HR.3051 Finvasive LIFE projec… <NA>                   Finland                2604
#> HR.3071 Biodiversity observat… <NA>                   Finland                 529
#> HR.3211 iNaturalist Suomi Fin… biota                  Finland             1670979
#> HR.3431 Butterflies in Finnis… Papilionoidea, Others  Finland              732007
#> HR.3491 LajiGIS: Species surv… Biota                  Finland              759186
#> HR.3553 LajiGIS: Species moni… Biota                  Finland              797208
#> HR.3691 eBird                  Aves                   Finland             2417556
#> HR.3791 Finnish invasive spec… Biota                  Finland                8099
#> HR.39   Winter Bird Census     Aves, Mammalia         Finland             1608783
#> HR.3911 Bumblebee census       Bumblebees             Finland               52893
#> HR.3991 Waterbird counts, Luo… Aves                   Finland               54931
#> HR.3992 Waterbird counts, Luk… Aves                   Finland               54782
#> HR.4011 Salmonidae in streams  Salmonoidei            Finland               25263
#> HR.4051 LajiGIS: Species moni… Aquila chrysaetos; Ha… Finland                9812
#> HR.4091 Retkikasvio            <NA>                   Finland                 220
#> HR.4191 Porvoo Museum / Colle… Lepidoptera            Finland               10417
#> HR.4251 LajiGIS: Species mapp… Biota                  Finland              638169
#> HR.435  Löydös Open Invasive … Biota                  Finland               27935
#> HR.4352 NFI rare tree species  <NA>                   Finland                 992
#> HR.4374 Porvoo Museum / Pisces <NA>                   Finland                   1
#> HR.4379 Porvoo Museum / Amphi… <NA>                   Finland                   5
#> HR.4386 Porvoo Museum / Oology <NA>                   Finland                 522
#> HR.4412 Tiira.fi: The Fourth … Aves                   Finland              819404
#> HR.4471 Fourth Finnish Bird A… Aves                   Finland              365368
#> HR.4511 Finnish National Moth… Bombycoidea, Noctuoid… Finland             1224173
#> HR.4611 Observations by FCG F… <NA>                   Finland                1591
#> HR.4612 Pollinator monitoring… Insecta                Finland               16692
#> HR.4672 Observations from pub… Biota                  Finland                 127
#> HR.4711 Flying squirrel monit… Pteromys volans        Finland                2509
#> HR.49   Finnish birds of prey… Accipitridae, Falconi… Finland              193879
#> HR.4991 Bird atlas observatio… Aves                   Finland               15757
#> HR.50   White-tailed Sea Eagl… Haliaeetus albicilla   Finland               32090
#> HR.5095 Dragonfly - complete … Odonata                Finland                1066
#> HR.5155 Observations from ele… Biota                  Finland                1423
#> HR.5196 Pollinator monitoring… Insecta                Finland               16571
#> HR.5235 Finnish butterflies -… Lepidoptera, Papilino… Finland                 751
#> HR.5236 Charismatic flowering… Tracheophyta           Finland               16849
#> HR.5255 Diurnal moths - compl… Lepidoptera            Finland                  36
#> HR.5256 Bumblebees - complete… Hymenoptera            Finland                 461
#> HR.5257 Amphibia and reptilia… Chordata               Finland                  91
#> HR.5258 Subarctic plants - co… Tracheophyta           Finland                  66
#> HR.5259 Macrolichens - comple… <NA>                   Finland                 286
#> HR.5260 Bracket fungi, redlis… <NA>                   Finland                  33
#> HR.5535 Observations of invas… <NA>                   Finland                6656
#> HR.5555 Observations by the f… <NA>                   Finland                3729
#> HR.5575 Noteworthy macrofungi… Fungi                  Finland                 816
#> HR.5795 Dataset from Luke to … Aves                   Finland               22536
#> HR.5835 Crowdsorsa mapping an… Plantae                Finland               41425
#> HR.5895 Oma riista -observati… Aves                   Finland               35965
#> HR.5938 Willow grouse countin… Lagopus lagopus        Finland                1067
#> HR.60   Monitoring scheme of … Aves, Mammalia         Finland              956964
#> HR.6058 Coll. Lilja Insects    <NA>                   Finland                 111
#> HR.627  Alien mammal species … Invasive alien mammal… Finland                4255
#> HR.64   Finnish bird nest rec… Aves                   Finland              973502
#> HR.6678 Finnish Hemiptera Wor… Hemiptera              Finland              142664
#> HR.6778 Line transect censuse… Aves                   Finland             1188179
#> HR.7080 LajiGIS: Species surv… Biota                  Finland              140389
#> HR.808  E. Sjöholm's butterfl… Lepidoptera            Finland                4946
#> HR.847  Atlas of amphibians a… Amphibia, Reptilia     Finland                7747
#> HR.95   Osprey monitoring      Pandion haliaetus      Finland              102550

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

``` r
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

``` r
finbif_collections(is_part_of == "HR.128", supercollections = TRUE)
```


<details closed>
<summary> Click to show/hide output. </summary>

```r

#>         collection_name abbreviation description online_url has_children is_part_of
#> HR.129  Collections of… <NA>         Herbarium … <NA>        TRUE        HR.128    
#> HR.160  Zoological col… <NA>         The collec… http://ww…  TRUE        HR.128    
#> HR.173  Luomus monitor… <NA>         Monitoring… <NA>        TRUE        HR.128    
#> HR.1849 Genomic resour… <NA>         Genomic re… <NA>        TRUE        HR.128    
#> HR.203  Löydös Open Fi… <NA>         A service … https://l… FALSE        HR.128    
#> HR.2169 Geological col… <NA>         All the ge… <NA>        TRUE        HR.128    
#> HR.435  Löydös Open In… <NA>         A service … https://l… FALSE        HR.128    
#> HR.447  Hatikka.fi obs… <NA>         Hatikka.fi… http://ha… FALSE        HR.128    
#> HR.48   Bird ringing a… TIPU         Dataset co… <NA>       FALSE        HR.128    
#> HR.7443 Luomus Botanic… <NA>         Living col… <NA>        TRUE        HR.128    
#>         data_quality methods   collection_type taxonomic_coverage geographic_coverage
#> HR.129  MY.dataQual… <NA>      MY.collectionT… <NA>               <NA>               
#> HR.160  MY.dataQual… <NA>      MY.collectionT… Animalia           World              
#> HR.173  MY.dataQual… <NA>      MY.collectionT… <NA>               Finland            
#> HR.1849 MY.dataQual… Sampling… MY.collectionT… Biota              World              
#> HR.203  <NA>         <NA>      MY.collectionT… biota              world              
#> HR.2169 MY.dataQual… <NA>      MY.collectionT… <NA>               <NA>               
#> HR.435  MY.dataQual… <NA>      MY.collectionT… Biota              Finland            
#> HR.447  MY.dataQual… <NA>      MY.collectionT… Biota              World              
#> HR.48   MY.dataQual… <NA>      MY.collectionT… Aves               Ringing data: Finl…
#> HR.7443 <NA>         <NA>      MY.collectionT… <NA>               <NA>               
#>         temporal_coverage secure_level count   
#> HR.129  <NA>              <NA>           679165
#> HR.160  1700 to present   MX.secureLe…      632
#> HR.173  1950-             <NA>          5898405
#> HR.1849 2000-             <NA>                1
#> HR.203  2013-             <NA>            71709
#> HR.2169 <NA>              <NA>                0
#> HR.435  2015-             <NA>            27935
#> HR.447  <NA>              <NA>          2011828
#> HR.48   1913-             <NA>         15792308
#> HR.7443 <NA>              <NA>                0

```

</details>
<br>
