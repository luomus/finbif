# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%"                                                                                                                                                                  
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 0"                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
       [4] "Records available: 911922 + 0"                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
       [5] "A data.frame [10 x 26]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "        scientific_name abundance lat_wgs84 lon_wgs84           date_time"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [7] "1  Cuculus canorus Lin…        NA  65.74447  28.19565 2023-06-11 12:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [8] "2  Bucephala clangula …  1         62.85694  21.39448 2023-01-01 12:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [9] "3  Parus major Linnaeu…  1         60.78113  22.40542 2023-01-01 12:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [10] "4  Carduelis flammea (…  1         69.04637  26.67011 2023-06-20 03:40:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [11] "5  Periparus ater (Lin…  1         60.78113  22.40542 2023-01-01 12:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [12] "6  Turdus iliacus Linn…        NA  60.27294  22.02446 2023-04-25 12:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [13] "7  Phylloscopus sibila…        NA  62.45113  23.39759 2023-06-13 12:17:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [14] "8  Regulus regulus (Li…  1         59.81393  20.75345 2023-01-01 12:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [15] "9  Lophophanes cristat…  1         65.08739  25.58938 2023-06-06 08:04:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [16] "10 Dendrocopos minor (…  1         64.41557  23.88369 2023-01-01 12:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
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
      A data.frame [0 x 1]
      [1] municipality
      <0 rows> (or 0-length row.names)

---

    Code
      plants
    Output
      Records downloaded: 232
      Records available: 232
      A data.frame [232 x 2]
         threatened_status orig_taxon_rank
      1                 NA           genus
      2                 NA         species
      3                 NA         species
      4                 NA         species
      5                 NA         species
      6                 NA         species
      7                 NA         species
      8                 NA         species
      9                 NA         species
      10                NA         species
      ...with 222 more records

---

    Code
      occ_print
    Output
      Records downloaded: 11
      Records available: 52104100
      A data.frame [11 x 1]
         informal_groups
      1       0 elements
      2        1 element
      3        1 element
      4        1 element
      5       2 elements
      6        1 element
      7       2 elements
      8        1 element
      9       2 elements
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

# fetching occurrences with async works

    Code
      list(fg, bg[["get_result"]]())
    Output
      [[1]]
      Records downloaded: 10
      Records available: 52104100
      A data.frame [10 x 12]
         record_id      scientific_name abundance lat_wgs84 lon_wgs84
      1       …1#3                 <NA>  1         66.58349  25.6617 
      2      …0#51 Hirundo rustica Lin…        NA  66.73361  27.78982
      3      …0#11 Larus argentatus Po…        NA  66.73361  27.78982
      4      …0#45 Muscicapa striata (…        NA  66.73361  27.78982
      5      …0#31 Anas acuta Linnaeus…        NA  66.73361  27.78982
      6       …0#9 Motacilla flava Lin…        NA  66.73361  27.78982
      7      …0#55 Gavia arctica (Linn…        NA  66.73361  27.78982
      8      …0#13 Grus grus (Linnaeus…        NA  66.73361  27.78982
      9      …0#15 Cygnus cygnus (Linn…        NA  66.73361  27.78982
      10     …0#29 Tringa glareola Lin…        NA  66.73361  27.78982
      ...with 0 more record and 7 more variables:
      date_time, coordinates_uncertainty, any_issues, requires_verification,
      requires_identification, record_reliability, record_quality
      
      [[2]]
      Records downloaded: 10
      Records available: 52104100
      A data.frame [10 x 12]
         record_id      scientific_name abundance lat_wgs84 lon_wgs84
      1       …1#3                 <NA>  1         66.58349  25.6617 
      2      …0#51 Hirundo rustica Lin…        NA  66.73361  27.78982
      3      …0#11 Larus argentatus Po…        NA  66.73361  27.78982
      4      …0#45 Muscicapa striata (…        NA  66.73361  27.78982
      5      …0#31 Anas acuta Linnaeus…        NA  66.73361  27.78982
      6       …0#9 Motacilla flava Lin…        NA  66.73361  27.78982
      7      …0#55 Gavia arctica (Linn…        NA  66.73361  27.78982
      8      …0#13 Grus grus (Linnaeus…        NA  66.73361  27.78982
      9      …0#15 Cygnus cygnus (Linn…        NA  66.73361  27.78982
      10     …0#29 Tringa glareola Lin…        NA  66.73361  27.78982
      ...with 0 more record and 7 more variables:
      date_time, coordinates_uncertainty, any_issues, requires_verification,
      requires_identification, record_reliability, record_quality
      

