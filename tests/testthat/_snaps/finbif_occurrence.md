# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%"                                                                                                                                                                  
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 0"                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
       [4] "Records available: 15782 + 0"                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
       [5] "A data.frame [10 x 24]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "        scientific_name abundance lat_wgs84 lon_wgs84           date_time"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [7] "1  Buteo buteo (Linnae…  1         62.62386  24.07332 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [8] "2  Cyanistes caeruleus…  4         62.33471  25.74237 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [9] "3  Accipiter gentilis …  1         61.28832  22.79833 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [10] "4  Sylvia communis (La…  5         61.075    28.57161 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [11] "5  Parus major Linnaeu…  9         61.16846  25.78908 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [12] "6  Parus major Linnaeu…  10        60.9918   26.16538 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [13] "7  Falco tinnunculus L…  1         60.39865  23.09532 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [14] "8  Pandion haliaetus (…  1               NA        NA 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [15] "9  Anthus trivialis (L…  1         60.6373   22.15136 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [16] "10 Lagopus lagopus (Li…  1         65.0118   29.43561 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [17] "...with 0 more record and 19 more variables:"                                                                                                                                                                                                                                                                                                                                                                                                                                                          
      [18] "coordinates_uncertainty, any_issues, requires_verification,"                                                                                                                                                                                                                                                                                                                                                                                                                                           
      [19] "requires_identification, record_reliability, record_quality, date_time_ISO8601,"                                                                                                                                                                                                                                                                                                                                                                                                                       
      [20] "collection, primary_habitat, epsg, occurrence_status, citation,"                                                                                                                                                                                                                                                                                                                                                                                                                                       
      [21] "red_list_status, informal_groups, common_name, restriction_reason, atlas_code,"                                                                                                                                                                                                                                                                                                                                                                                                                        
      [22] "atlas_class, NA"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       

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
      1                 NA         species
      2                 NA         species
      3                 NA         species
      4                 NA         species
      5                 NA           genus
      6                 NA         species
      7                 NA           genus
      8                 NA           genus
      9                 NA         species
      10                NA           genus
      ...with 35 more records

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

