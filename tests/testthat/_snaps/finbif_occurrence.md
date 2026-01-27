# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 10"                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
       [4] "Records available: 1295881 + 31129"                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
       [5] "A data.frame [20 x 26]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "        scientific_name abundance lat_wgs84 lon_wgs84           date_time"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [7] "1  Larus argentatus Po…        NA  61.26206  27.64908 2023-06-21 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [8] "2  Corvus corone corni…  1         64.94818  25.43856 2023-05-17 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [9] "3  Carduelis carduelis…        NA  61.19729  24.11426 2023-05-26 01:02:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [10] "4  Regulus regulus (Li…        NA  60.2601   24.91945 2023-05-06 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [11] "5  Pandion haliaetus (…  1         62.60199  28.457   2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [12] "6  Tringa nebularia (G…        NA  65.26906  25.97658 2023-07-06 09:01:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [13] "7  Passer domesticus (…  1         64.1728   28.18211 2023-06-15 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [14] "8  Chloris chloris (Li…  5         66.79536  23.92593 2023-04-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [15] "9  Cygnus cygnus (Linn…        NA  61.64031  22.56165 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [16] "10 Saxicola rubetra (L…        NA  64.21821  25.65757 2023-07-01 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
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
      Records downloaded: 42
      Records available: 42
      A data.frame [42 x 2]
         threatened_status orig_taxon_rank
      1               <NA>           genus
      2               <NA>         species
      3               <NA>         species
      4               <NA>      subspecies
      5               <NA>         species
      6               <NA>       aggregate
      7               <NA>      subspecies
      8         Threatened         species
      9               <NA>         species
      10              <NA>         species
      ...with 32 more records

---

    Code
      occ_print
    Output
      Records downloaded: 11
      Records available: 59105195
      A data.frame [11 x 1]
         informal_groups
      1       3 elements
      2        1 element
      3        1 element
      4        1 element
      5        1 element
      6        1 element
      7       2 elements
      8       3 elements
      9        1 element
      10       1 element
      ...with 1 more record

# fetching occurrences with date filters works

    Code
      date_filters
    Output
      Records downloaded: 5 + 5 + 5 + 5 + 5 + 5 + 5
      Records available: 1524650 + 1141115 + 1667736 + 902 + 1667736 + 1667736 + 13…
      A data.frame [20 x 12]
                       record_id      scientific_name abundance lat_wgs84 lon_wgs84
      1  …3691/OBS779129438_Unit Turdus merula Linna…  10        60.40945  22.27458
      2  …3691/OBS779129435_Unit Cyanistes caeruleus…  20        60.40945  22.27458
      3  …3691/OBS565512119_Unit Cyanistes caeruleus…  3         60.47784  24.23513
      4  …3691/OBS779129439_Unit Chloris chloris (Li…  10        60.40945  22.27458
      5  …3691/OBS779129437_Unit Turdus pilaris Linn…  20        60.40945  22.27458
      6   …49/93344/G.119815_PUL Falco tinnunculus L…  6         65.96698  24.64551
      7   …49/93332/G.119801_PUL Falco tinnunculus L…  3         65.9621   24.60614
      8   …49/92487/G.118629_PUL Falco tinnunculus L…  6         65.72931  24.77294
      9              …49/92487_U Falco tinnunculus L…  1         65.72931  24.77294
      10  …49/92319/G.118417_PUL Falco tinnunculus L…  4         65.87306  24.51957
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
      1         Heard  469599    175     
      2   Observation  193671    170     
      3          Seen  118363    204     

# can compute a var from id when there are zero records

    Code
      no_record_compute_id
    Output
      Records downloaded: 0
      Records available: 0
      A data.frame [0 x 1]
      [1] finnish_municipality
      <0 rows> (or 0-length row.names)

