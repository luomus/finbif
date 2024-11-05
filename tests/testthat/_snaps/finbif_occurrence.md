# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 10"                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
       [4] "Records available: 914607 + 30983"                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
       [5] "A data.frame [20 x 26]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "        scientific_name abundance lat_wgs84 lon_wgs84           date_time"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [7] "1  Sterna hirundo Linn…  1         60.16747  24.74513 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [8] "2  Carduelis chloris (…  1         59.9349   24.31358 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [9] "3  Actitis hypoleucos …        NA  62.32828  25.16375 2023-05-11 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [10] "4  Carduelis spinus (L…        NA  66.42788  25.78198 2023-06-06 07:30:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [11] "5  Larus ridibundus (L…        NA  60.89322  25.24684 2023-07-22 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [12] "6  Emberiza rustica (P…  1         64.41557  23.88369 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [13] "7  Muscicapa striata (…        NA  59.98152  24.39941 2023-06-24 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [14] "8  Pinicola enucleator…        NA  64.55637  29.81218 2023-02-20 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [15] "9  Sylvia borin (Bodda…        NA  63.93279  24.44753 2023-06-12 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [16] "10 Parus major Linnaeu…  12        60.98529  28.56718 2023-03-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
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
      A data.frame [0 x 3]
      [1] municipality local_area   set         
      <0 rows> (or 0-length row.names)

---

    Code
      plants
    Output
      Records downloaded: 51
      Records available: 51
      A data.frame [51 x 2]
         threatened_status orig_taxon_rank
      1                 NA         species
      2                 NA         species
      3                 NA         species
      4                 NA         species
      5                 NA         species
      6                 NA         species
      7                 NA         species
      8                 NA         species
      9                 NA         species
      10                NA         species
      ...with 41 more records

---

    Code
      occ_print
    Output
      Records downloaded: 11
      Records available: 52796498
      A data.frame [11 x 1]
         informal_groups
      1       3 elements
      2       4 elements
      3       4 elements
      4       3 elements
      5       3 elements
      6        1 element
      7       3 elements
      8        1 element
      9        1 element
      10       1 element
      ...with 1 more record

# fetching occurrences with date filters works

    Code
      date_filters
    Output
      Records downloaded: 5 + 5 + 5 + 5 + 5 + 5 + 5
      Records available: 1499724 + 1124459 + 1641155 + 893 + 1641155 + 1641155 + 12…
      A data.frame [20 x 12]
                         record_id      scientific_name abundance lat_wgs84 lon_wgs84
      1        …tun.fi/JX.113239#6 Pica pica (Linnaeus…  17        61.49315  23.71047
      2        …tun.fi/JX.113239#8 Corvus corone corni…  81        61.49315  23.71047
      3       …tun.fi/JX.113239#21 Regulus regulus (Li…  17        61.49315  23.71047
      4       …tun.fi/JX.113239#18 Poecile montanus (C…  7         61.49315  23.71047
      5        …tun.fi/JX.113239#3 Columba livia Gmeli…  37        61.49315  23.71047
      6        …tun.fi/MY.17232675 Archips podana (Sco…  1         60.21988  24.93102
      7          …tun.fi/MY.890425 Dicranum flexicaule…        NA  61.1      127.35  
      8          …tun.fi/MY.890417 Dicranum fuscescens…        NA  61.1      127.35  
      9  …id.luomus.fi/MY.15425361 Udea hamalis (Thunb…  1         60.46742  26.27887
      10  …id.luomus.fi/MY.9880728 Hypena crassalis (F…  1         60.45338  26.17924
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
      1         Heard  445358    173     
      2   Observation  190973    169     
      3          Seen  113109    204     

