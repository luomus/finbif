# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%"                                                                                                                                                                  
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 0"                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
       [4] "Records available: 912690 + 0"                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
       [5] "A data.frame [10 x 26]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "        scientific_name abundance lat_wgs84 lon_wgs84           date_time"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [7] "1  Bucephala clangula …        NA  59.97617  23.36208 2023-05-13 03:09:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [8] "2  Sylvia borin (Bodda…        NA  62.24501  25.74611 2023-05-21 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [9] "3  Podiceps cristatus …        NA  61.65389  22.98214 2023-07-09 16:43:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [10] "4  Parus major Linnaeu…  1         61.75392  27.9434  2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [11] "5  Larus canus Linnaeu…        NA  60.8907   25.06274 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [12] "6  Ficedula hypoleuca …  1         68.61487  28.10121 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [13] "7  Turdus pilaris Linn…        NA  62.49144  24.18337 2023-05-26 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [14] "8  Loxia pytyopsittacu…  1         67.69676  26.68912 2023-06-14 00:27:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [15] "9  Phylloscopus trochi…  1         60.85665  27.91684 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [16] "10 Acrocephalus scirpa…        NA  60.43023  21.45447 2023-03-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [17] "...with 0 more record and 21 more variables:"                                                                                                                                                                                                                                                                                                                                                                                                                                                          
      [18] "coordinates_uncertainty, any_issues, requires_verification,"                                                                                                                                                                                                                                                                                                                                                                                                                                           
      [19] "requires_identification, record_reliability, record_quality, duration,"                                                                                                                                                                                                                                                                                                                                                                                                                                
      [20] "date_time_ISO8601, collection, primary_habitat, epsg, occurrence_status,"                                                                                                                                                                                                                                                                                                                                                                                                                              
      [21] "citation, red_list_status, region, informal_groups, common_name,"                                                                                                                                                                                                                                                                                                                                                                                                                                      
      [22] "restriction_reason, atlas_code, atlas_class, NA"                                                                                                                                                                                                                                                                                                                                                                                                                                                       

---

    Code
      hr778
    Output
      Records downloaded: 5
      Records available: 5
      A data.frame [5 x 12]
        record_id      scientific_name abundance lat_wgs84 lon_wgs84
      1 …10888538 Ceramium tenuicorne…        NA  58.79118  17.60312
      2  …2475121 Batrachospermum tur…        NA  60.50838  19.73372
      3  …2475348 Furcellaria lumbric…        NA  60.50838  19.73372
      4  …2475351 Batrachospermum gel…        NA  60.50838  19.73372
      5  …2475345 Lemanea fluviatilis…        NA  60.31924  24.96965
      ...with 0 more record and 7 more variables:
      date_time, coordinates_uncertainty, any_issues, requires_verification,
      requires_identification, record_reliability, record_quality

---

    Code
      hr778_no_records
    Output
      Records downloaded: 0 + 0
      Records available: 0 + 0
      A data.frame [0 x 2]
      [1] municipality set         
      <0 rows> (or 0-length row.names)

---

    Code
      plants
    Output
      Records downloaded: 26
      Records available: 26
      A data.frame [26 x 2]
         threatened_status orig_taxon_rank
      1               <NA>         species
      2               <NA>         species
      3               <NA>         species
      4               <NA>         species
      5               <NA>         species
      6               <NA>         species
      7               <NA>         species
      8               <NA>           genus
      9    Near Threatened         species
      10              <NA>         species
      ...with 16 more records

---

    Code
      occ_print
    Output
      Records downloaded: 11
      Records available: 52323067
      A data.frame [11 x 1]
         informal_groups
      1       4 elements
      2        1 element
      3        1 element
      4        1 element
      5       4 elements
      6        1 element
      7        1 element
      8        1 element
      9        1 element
      10       1 element
      ...with 1 more record

# fetching occurrences with date filters works

    Code
      date_filters
    Output
      Records downloaded: 5 + 5 + 5 + 5 + 5 + 5 + 5
      Records available: 1496994 + 1122246 + 1638330 + 892 + 1638330 + 1638330 + 12…
      A data.frame [20 x 12]
                         record_id      scientific_name abundance lat_wgs84 lon_wgs84
      1        …tun.fi/JX.113239#6 Pica pica (Linnaeus…  17        61.49315  23.71047
      2        …tun.fi/JX.113239#8 Corvus corone corni…  81        61.49315  23.71047
      3       …tun.fi/JX.113239#21 Regulus regulus (Li…  17        61.49315  23.71047
      4       …tun.fi/JX.113239#18 Poecile montanus (C…  7         61.49315  23.71047
      5        …tun.fi/JX.113239#3 Columba livia Gmeli…  37        61.49315  23.71047
      6  …id.luomus.fi/MY.16999300       Bryonora Poelt        NA  61.1      127.35  
      7        …tun.fi/MY.16776429 Hydrelia flammeolar…  1         63.5778   27.1899 
      8        …tun.fi/MY.16776393 Odezia atrata (Linn…  1         63.5778   27.1899 
      9        …tun.fi/MY.16776273 Xanthorhoe montanat…  1         63.5778   27.1899 
      10       …tun.fi/MY.16508498 Odezia atrata (Linn…  1         63.55754  27.1138 
      ...with 10 more records and 7 more variables:
      date_time, coordinates_uncertainty, any_issues, requires_verification,
      requires_identification, record_reliability, record_quality

# fetching aggregated occurrences works

    Code
      record_basis_aggregate
    Output
      Records downloaded: 3
      Records available: 3
      A data.frame [3 x 3]
        basisOfRecord n_records n_species
      1         Heard  444606    173     
      2   Observation  189887    168     
      3          Seen  112976    204     

