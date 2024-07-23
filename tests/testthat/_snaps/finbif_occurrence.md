# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%"                                                                                                                                                                  
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 0"                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
       [4] "Records available: 911937 + 0"                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
       [5] "A data.frame [10 x 26]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "        scientific_name abundance lat_wgs84 lon_wgs84           date_time"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [7] "1  Ficedula hypoleuca …  1         60.32389  23.64777 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [8] "2  Vanellus vanellus (…        NA  64.84592  25.62636 2023-07-11 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [9] "3  Circus cyaneus (Lin…        NA  62.77092  24.74449 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [10] "4  Periparus ater (Lin…  1         59.88642  22.52974 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [11] "5  Haliaeetus albicill…        NA  60.06753  24.21296 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [12] "6  Branta canadensis (…        NA  60.45571  25.064   2023-05-10 13:45:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [13] "7  Fringilla montifrin…        NA  67.08489  25.27044 2023-06-30 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [14] "8  Stercorarius longic…        NA  69.16059  21.57647 2023-07-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [15] "9  Lagopus lagopus (Li…        NA  69.5111   25.84403 2023-01-01 11:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [16] "10 Parus major Linnaeu…  1         61.72772  24.15874 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
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
      Records downloaded: 25
      Records available: 25
      A data.frame [25 x 2]
         threatened_status orig_taxon_rank
      1         Threatened         species
      2               <NA>         species
      3               <NA>         species
      4          Statutory         species
      5               <NA>         species
      6               <NA>         species
      7               <NA>         species
      8               <NA>         species
      9               <NA>         species
      10              <NA>         species
      ...with 15 more records

---

    Code
      occ_print
    Output
      Records downloaded: 11
      Records available: 52111306
      A data.frame [11 x 1]
         informal_groups
      1        1 element
      2        1 element
      3       4 elements
      4       4 elements
      5        1 element
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
      Records available: 1494740 + 1120348 + 1635924 + 892 + 1635924 + 1635924 + 12…
      A data.frame [20 x 12]
             record_id      scientific_name abundance lat_wgs84 lon_wgs84
      1   …JX.113239#6 Pica pica (Linnaeus…  17        61.49315  23.71047
      2   …JX.113239#8 Corvus corone corni…  81        61.49315  23.71047
      3  …JX.113239#21 Regulus regulus (Li…  17        61.49315  23.71047
      4  …JX.113239#18 Poecile montanus (C…  7         61.49315  23.71047
      5   …JX.113239#3 Columba livia Gmeli…  37        61.49315  23.71047
      6   …MY.16508498 Odezia atrata (Linn…  1         63.55754  27.1138 
      7   …MY.16508510 Odezia atrata (Linn…  1         63.55754  27.1138 
      8   …MY.16479013 Aphantopus hyperant…  1         63.55754  27.1138 
      9   …MY.16475385 Polyommatus amandus…  1         63.55754  27.1138 
      10  …MY.16475481 Polyommatus amandus…  1         63.55754  27.1138 
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
      1         Heard  443951    173     
      2   Observation  189727    168     
      3          Seen  112839    204     

