# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%"                                                                                                                                                                  
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 0"                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
       [4] "Records available: 15782 + 0"                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
       [5] "A data.frame [10 x 25]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "        scientific_name abundance lat_wgs84 lon_wgs84           date_time"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [7] "1  Ficedula hypoleuca …  5         63.82174  23.44134 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [8] "2  Haliaeetus albicill…  1         60.05752  21.15737 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [9] "3  Parus major Linnaeu…        NA  60.18478  24.94956 2023-12-23 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [10] "4  Aegolius funereus (…  1         62.5697   22.13113 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [11] "5  Strix aluco Linnaeu…  1         60.1491   23.84576 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [12] "6  Passer domesticus (…  1         60.17483  24.93352 2023-11-28 12:59:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [13] "7  Pandion haliaetus (…  1         60.9941   26.71968 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [14] "8  Tringa nebularia (G…  1         62.48324  30.19739 2023-01-01 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [15] "9  Pandion haliaetus (…  1         62.78322  25.72329 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [16] "10 Pandion haliaetus (…  1         61.33323  24.66176 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [17] "...with 0 more record and 20 more variables:"                                                                                                                                                                                                                                                                                                                                                                                                                                                          
      [18] "coordinates_uncertainty, any_issues, requires_verification,"                                                                                                                                                                                                                                                                                                                                                                                                                                           
      [19] "requires_identification, record_reliability, record_quality, date_time_ISO8601,"                                                                                                                                                                                                                                                                                                                                                                                                                       
      [20] "collection, primary_habitat, epsg, occurrence_status, citation,"                                                                                                                                                                                                                                                                                                                                                                                                                                       
      [21] "red_list_status, region, informal_groups, common_name, restriction_reason,"                                                                                                                                                                                                                                                                                                                                                                                                                            
      [22] "atlas_code, atlas_class, NA"                                                                                                                                                                                                                                                                                                                                                                                                                                                                           

---

    Code
      hr778
    Output
      Records downloaded: 1
      Records available: 1
      A data.frame [1 x 12]
                           record_id      scientific_name abundance lat_wgs84
      1 http://mus.utu.fi/MY.2475121 Batrachospermum tur…        NA  60.50838
      ...with 0 more record and 8 more variables:
      lon_wgs84, date_time, coordinates_uncertainty, any_issues,
      requires_verification, requires_identification, record_reliability,
      record_quality

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
      Records downloaded: 45
      Records available: 45
      A data.frame [45 x 2]
         threatened_status orig_taxon_rank
      1               <NA>         species
      2               <NA>         species
      3               <NA>         species
      4               <NA>         species
      5               <NA>         species
      6               <NA>         species
      7               <NA>           genus
      8               <NA>         species
      9               <NA>         species
      10        Threatened      subspecies
      ...with 35 more records

---

    Code
      occ_print
    Output
      Records downloaded: 11
      Records available: 3243278
      A data.frame [11 x 1]
         informal_groups
      1       4 elements
      2       4 elements
      3       4 elements
      4        1 element
      5       2 elements
      6       4 elements
      7       2 elements
      8        1 element
      9        1 element
      10       1 element
      ...with 1 more record

# fetching occurrences with date filters works

    Code
      date_filters
    Output
      Records downloaded: 5 + 5 + 5 + 5 + 5 + 5 + 5
      Records available: 53407 + 35115 + 61343 + 5 + 61343 + 61343 + 10768
      A data.frame [20 x 12]
                         record_id      scientific_name abundance lat_wgs84 lon_wgs84
      1              …JX.144207#14 Poecile montanus (C…  23        65.73193  24.70732
      2              …JX.144207#12 Loxia leucoptera J.…  24        65.73193  24.70732
      3              …JX.144207#17 Cinclus cinclus (Li…  1         65.73193  24.70732
      4              …JX.144207#13 Periparus ater (Lin…  4         65.73193  24.70732
      5               …JX.144207#5 Dendrocopos major (…  4         65.73193  24.70732
      6  …KE.921/LGE.214984/364407 Nola karelica Tengs…  17        61.147    24.30393
      7  …KE.921/LGE.120229/269652 Carex appropinquata…        NA  60.9957   24.22772
      8   …KE.921/LGE.81792/224044 Carex diandra Schra…        NA  64.64172  24.53726
      9   …KE.921/LGE.81792/223095     Cicuta virosa L.        NA  64.64172  24.53726
      10   …KE.921/LGE.81792/81792 Dactylorhiza incarn…  7         64.64172  24.53726
      ...with 10 more records and 7 more variables:
      date_time, coordinates_uncertainty, any_issues, requires_verification,
      requires_identification, record_reliability, record_quality

# fetching aggregated occurrences works

    Code
      record_basis_aggregate
    Output
      Records downloaded: 0
      Records available: 0
      A data.frame [0 x 3]
      [1] basisOfRecord n_records     n_species    
      <0 rows> (or 0-length row.names)

