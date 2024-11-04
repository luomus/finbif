# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 10"                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
       [4] "Records available: 15781 + 728"                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
       [5] "A data.frame [20 x 26]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "        scientific_name abundance lat_wgs84 lon_wgs84           date_time"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [7] "1  Ficedula hypoleuca …  5         64.0057   23.62257 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [8] "2  Pica pica (Linnaeus…        NA  61.12005  26.31582 2023-01-23 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [9] "3  Pandion haliaetus (…  1         60.69503  24.15879 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [10] "4  Parus major Linnaeu…  9         60.90207  26.16773 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [11] "5  Pandion haliaetus (…  1         63.23659  21.91926 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [12] "6  Passer montanus (Li…  4         60.54187  25.99479 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [13] "7  Haliaeetus albicill…  1         59.71766  18.98764 2023-01-01 11:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [14] "8  Apus apus (Linnaeus…  1         60.54187  25.99479 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [15] "9  Dryocopus martius (…  1         62.5697   22.13113 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [16] "10 Phoenicurus phoenic…  1         67.7927   29.48489 2023-01-01 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [17] "...with 10 more records and 21 more variables:"                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      [18] "coordinates_uncertainty, any_issues, requires_verification,"                                                                                                                                                                                                                                                                                                                                                                                                                                           
      [19] "requires_identification, record_reliability, record_quality, duration,"                                                                                                                                                                                                                                                                                                                                                                                                                                
      [20] "date_time_ISO8601, collection, primary_habitat, epsg, occurrence_status,"                                                                                                                                                                                                                                                                                                                                                                                                                              
      [21] "citation, red_list_status, region, informal_groups, common_name,"                                                                                                                                                                                                                                                                                                                                                                                                                                      
      [22] "restriction_reason, atlas_code, atlas_class, country"                                                                                                                                                                                                                                                                                                                                                                                                                                                  

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
      A data.frame [0 x 3]
      [1] municipality local_area   set         
      <0 rows> (or 0-length row.names)

---

    Code
      plants
    Output
      Records downloaded: 34
      Records available: 34
      A data.frame [34 x 2]
         threatened_status orig_taxon_rank
      1               <NA>           genus
      2               <NA>           genus
      3          Statutory         species
      4               <NA>         species
      5    Near Threatened         species
      6          Statutory         species
      7               <NA>           genus
      8          Statutory         species
      9               <NA>         species
      10              <NA>         species
      ...with 24 more records

---

    Code
      occ_print
    Output
      Records downloaded: 11
      Records available: 3243303
      A data.frame [11 x 1]
         informal_groups
      1        1 element
      2        1 element
      3        1 element
      4        1 element
      5       4 elements
      6       4 elements
      7       4 elements
      8        1 element
      9       2 elements
      10      4 elements
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

