# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 10"                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
       [4] "Records available: 16240 + 728"                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
       [5] "A data.frame [20 x 27]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "        scientific_name abundance lat_wgs84 lon_wgs84           date_time"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [7] "1  Ficedula hypoleuca …  7         60.05509  23.67509 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [8] "2  Haliaeetus albicill…  1         60.46056  20.17322 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [9] "3  Pandion haliaetus (…  3         62.96569  27.88366 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [10] "4  Astur gentilis (Lin…  2         63.22936  21.72114 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [11] "5  Anthus pratensis (L…  3         68.67996  23.91518 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [12] "6  Falco tinnunculus L…  1         61.5568   22.76201 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [13] "7  Pandion haliaetus (…  1         61.50557  24.27301 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [14] "8  Pandion haliaetus (…  1               NA        NA 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [15] "9  Falco tinnunculus L…  1         66.23843  28.10781 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [16] "10 Astur gentilis (Lin…  1         62.65102  26.02154 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [17] "...with 10 more records and 22 more variables:"                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      [18] "coordinates_uncertainty, any_issues, requires_verification,"                                                                                                                                                                                                                                                                                                                                                                                                                                           
      [19] "requires_identification, record_reliability, record_quality, duration,"                                                                                                                                                                                                                                                                                                                                                                                                                                
      [20] "date_time_ISO8601, collection, primary_habitat, epsg, occurrence_status,"                                                                                                                                                                                                                                                                                                                                                                                                                              
      [21] "citation, collection_code, red_list_status, region, informal_groups,"                                                                                                                                                                                                                                                                                                                                                                                                                                  
      [22] "common_name, restriction_reason, atlas_code, atlas_class, country"                                                                                                                                                                                                                                                                                                                                                                                                                                     

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
      Records downloaded: 50
      Records available: 50
      A data.frame [50 x 2]
         threatened_status orig_taxon_rank
      1                 NA         species
      2                 NA         species
      3                 NA         species
      4                 NA         species
      5                 NA         species
      6                 NA         species
      7                 NA          family
      8                 NA         species
      9                 NA           genus
      10                NA           genus
      ...with 40 more records

---

    Code
      occ_print
    Output
      Records downloaded: 11
      Records available: 3371875
      A data.frame [11 x 1]
         informal_groups
      1       2 elements
      2       4 elements
      3       4 elements
      4       4 elements
      5       4 elements
      6       4 elements
      7       4 elements
      8       4 elements
      9       4 elements
      10      4 elements
      ...with 1 more record

# fetching occurrences with date filters works

    Code
      date_filters
    Output
      Records downloaded: 5 + 5 + 5 + 5 + 5 + 5 + 5
      Records available: 60224 + 40231 + 68109 + 8 + 68109 + 68109 + 11374
      A data.frame [18 x 12]
                           record_id      scientific_name abundance lat_wgs84
      1  …id.herb.oulu.fi/MY.1962140 Tubulicrinis caloth…        NA  64.77997
      2  …id.herb.oulu.fi/MY.1962197 Skeletocutis papyra…        NA  64.81623
      3  …id.herb.oulu.fi/MY.1962210 Dacryobolus sudans …        NA  64.77997
      4  …id.herb.oulu.fi/MY.1962214 Botryobasidium subc…        NA  64.78254
      5  …id.herb.oulu.fi/MY.1962224 Phlebia segregata (…        NA  64.79727
      6  …id.herb.oulu.fi/MY.1962230 Jaapia ochroleuca (…        NA  64.78224
      7  …id.herb.oulu.fi/MY.1962232 Serpula himantioide…        NA  64.78224
      8  …id.herb.oulu.fi/MY.1962256 Anthoporia albobrun…        NA  64.78888
      9      …tun.fi/HR.4113/A.322_U Strix aluco Linnaeu…  1         61.44685
      10     …tun.fi/HR.4113/A.349_U Strix aluco Linnaeu…  1         61.51824
      ...with 8 more records and 8 more variables:
      lon_wgs84, date_time, coordinates_uncertainty, any_issues,
      requires_verification, requires_identification, record_reliability,
      record_quality

# fetching aggregated occurrences works

    Code
      record_basis_aggregate
    Output
      Records downloaded: 0
      Records available: 0
      A data.frame [0 x 3]
      [1] basisOfRecord n_records     n_species    
      <0 rows> (or 0-length row.names)

# can compute a var from id when there are zero records

    Code
      no_record_compute_id
    Output
      Records downloaded: 0
      Records available: 0
      A data.frame [0 x 1]
      [1] finnish_municipality
      <0 rows> (or 0-length row.names)

