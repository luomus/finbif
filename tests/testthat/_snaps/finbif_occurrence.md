# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 10"                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
       [4] "Records available: 1301342 + 31177"                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
       [5] "A data.frame [20 x 28]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "        scientific_name abundance lat_wgs84 lon_wgs84           date_time"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [7] "1  Parus major Linnaeu…  1         60.55434  22.3464  2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [8] "2  Parus major Linnaeu…  25        60.16874  22.21838 2023-01-07 07:05:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [9] "3  Chloris chloris (Li…  1         62.10782  26.79642 2023-02-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [10] "4  Turdus iliacus Linn…  1         61.852    25.179   2023-04-16 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [11] "5  Cygnus cygnus (Linn…  1         62.96802  23.7967  2023-06-01 01:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [12] "6  Regulus regulus (Li…  1         59.81393  20.75345 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [13] "7  Ficedula hypoleuca …  1         60.1731   25.10516 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [14] "8  Periparus ater (Lin…  1         60.26681  24.12653 2023-02-25 05:30:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [15] "9  Phylloscopus trochi…  1         62.76216  22.2478  2023-06-15 01:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [16] "10 Emberiza citrinella…        NA  60.21854  25.00521 2023-02-15 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [17] "...with 10 more records and 23 more variables:"                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      [18] "coordinates_uncertainty, any_issues, requires_verification,"                                                                                                                                                                                                                                                                                                                                                                                                                                           
      [19] "requires_identification, record_reliability, record_quality, duration,"                                                                                                                                                                                                                                                                                                                                                                                                                                
      [20] "date_time_ISO8601, collection, primary_habitat, epsg, occurrence_status,"                                                                                                                                                                                                                                                                                                                                                                                                                              
      [21] "citation, collection_code, red_list_status, region, informal_groups,"                                                                                                                                                                                                                                                                                                                                                                                                                                  
      [22] "common_name, restriction_reason, atlas_code, atlas_class, country,"                                                                                                                                                                                                                                                                                                                                                                                                                                    
      [23] "MY.pairCount"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          

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
      3  …2475345 Lemanea fluviatilis…        NA  60.31924  24.96965
      4  …2475348 Furcellaria lumbric…        NA  60.50838  19.73372
      5  …2475351 Batrachospermum gel…        NA  60.50838  19.73372
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
      Records downloaded: 56
      Records available: 56
      A data.frame [56 x 2]
         threatened_status orig_taxon_rank
      1          Statutory         species
      2               <NA>         species
      3               <NA>         species
      4               <NA>         species
      5               <NA>         species
      6               <NA>           genus
      7               <NA>         species
      8               <NA>         species
      9               <NA>           genus
      10              <NA>         species
      ...with 46 more records

---

    Code
      occ_print
    Output
      Records downloaded: 11
      Records available: 59646711
      A data.frame [11 x 1]
         informal_groups
      1       2 elements
      2       2 elements
      3       2 elements
      4       2 elements
      5       2 elements
      6       2 elements
      7       2 elements
      8       2 elements
      9       2 elements
      10      2 elements
      ...with 1 more record

# fetching occurrences with date filters works

    Code
      date_filters
    Output
      Records downloaded: 5 + 5 + 5 + 5 + 5 + 5 + 5
      Records available: 1530410 + 1146934 + 1674660 + 899 + 1674660 + 1674660 + 13…
      A data.frame [15 x 12]
                         record_id      scientific_name abundance lat_wgs84 lon_wgs84
      1  …herb.oulu.fi/MY.10079449 Polytrichum juniper…        NA  66.00408  28.20228
      2  …herb.oulu.fi/MY.10079491 Polytrichum pilifer…        NA  66.00406  28.20448
      3  …herb.oulu.fi/MY.10079494   Cladonia P. Browne        NA  66.00406  28.20448
      4  …herb.oulu.fi/MY.10079521 Polytrichum strictu…        NA  66.0077   28.19805
      5  …herb.oulu.fi/MY.10079581 Polytrichum jenseni…        NA  64.72938  29.21805
      6   …herb.oulu.fi/MY.4185479 Trametes betulina (…        NA  61.30691  24.77854
      7     …luomus.fi/MY.13883497 Phanerochaete lives…        NA  61.73571  25.74511
      8     …luomus.fi/MY.15939503 Resinicium bicolor …        NA  61.43146  23.57632
      9     …luomus.fi/MY.16033749 Skeletocutis amorph…        NA  61.62556  23.35387
      10    …luomus.fi/MY.16286825 Myotis brandtii (Ev…        NA  63.90816  30.22514
      ...with 5 more records and 7 more variables:
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
      1         Heard  469553    174     
      2   Observation  193569    169     
      3          Seen  118324    202     

# can compute a var from id when there are zero records

    Code
      no_record_compute_id
    Output
      Records downloaded: 0
      Records available: 0
      A data.frame [0 x 1]
      [1] finnish_municipality
      <0 rows> (or 0-length row.names)

