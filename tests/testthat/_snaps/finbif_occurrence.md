# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 10"                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
       [4] "Records available: 1287849 + 31102"                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
       [5] "A data.frame [20 x 27]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "        scientific_name abundance lat_wgs84 lon_wgs84           date_time"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [7] "1  Anas clypeata (Linn…  2         62.75198  30.22646 2023-04-28 02:44:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [8] "2  Anas crecca Linnaeu…        NA  66.72376  25.06904 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [9] "3  Anas crecca Linnaeu…        NA  60.1895   24.8191  2023-08-20 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [10] "4  Rallus aquaticus Li…        NA  61.23282  24.11037 2023-06-13 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [11] "5  Haliaeetus albicill…  1         64.22346  26.48143 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [12] "6  Somateria mollissim…  5         61.41265  21.2805  2023-05-27 11:05:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [13] "7  Carduelis flammea (…  1         60.8566   26.07691 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [14] "8  Lullula arborea (Li…  2         59.78241  21.36744 2023-10-08 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [15] "9  Pandion haliaetus (…  2         61.14701  24.30401 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [16] "10 Anthus trivialis (L…  1         62.95931  28.54451 2023-06-11 00:35:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [17] "...with 10 more records and 22 more variables:"                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      [18] "coordinates_uncertainty, any_issues, requires_verification,"                                                                                                                                                                                                                                                                                                                                                                                                                                           
      [19] "requires_identification, record_reliability, record_quality, duration,"                                                                                                                                                                                                                                                                                                                                                                                                                                
      [20] "date_time_ISO8601, collection, primary_habitat, epsg, occurrence_status,"                                                                                                                                                                                                                                                                                                                                                                                                                              
      [21] "citation, red_list_status, region, informal_groups, common_name,"                                                                                                                                                                                                                                                                                                                                                                                                                                      
      [22] "restriction_reason, atlas_code, atlas_class, country, MY.pairCount"                                                                                                                                                                                                                                                                                                                                                                                                                                    

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
      Records downloaded: 43
      Records available: 43
      A data.frame [43 x 2]
         threatened_status orig_taxon_rank
      1               <NA>         species
      2               <NA>           genus
      3               <NA>    nothospecies
      4               <NA>         species
      5               <NA>         species
      6          Statutory         species
      7               <NA>         species
      8               <NA>         species
      9               <NA>         species
      10              <NA>         species
      ...with 33 more records

---

    Code
      occ_print
    Output
      Records downloaded: 11
      Records available: 56329960
      A data.frame [11 x 1]
         informal_groups
      1        1 element
      2        1 element
      3        1 element
      4        1 element
      5        1 element
      6       3 elements
      7        1 element
      8       3 elements
      9       4 elements
      10      4 elements
      ...with 1 more record

# fetching occurrences with date filters works

    Code
      date_filters
    Output
      Records downloaded: 5 + 5 + 5 + 5 + 5 + 5 + 5
      Records available: 1508980 + 1131619 + 1651257 + 902 + 1651257 + 1651257 + 13…
      A data.frame [20 x 12]
                   record_id      scientific_name abundance lat_wgs84 lon_wgs84
      1  …KE.67/9586110#Unit Larus ridibundus (L…  1         53.21667  6.566667
      2  …KE.67/9586592#Unit Larus ridibundus (L…  1         53.2      5.783333
      3  …KE.67/9590259#Unit Larus ridibundus (L…  1         56.15     10.21667
      4  …KE.67/9590262#Unit Larus ridibundus (L…  1         56.16667  10.21667
      5  …KE.67/9605091#Unit Larus ridibundus (L…  1         52.1      5.116667
      6        …MKC.31547508 Alchemilla hirsutic…        NA  63.91587  30.07715
      7        …MKC.31547575 Alchemilla hirsutic…        NA  63.91497  30.07705
      8        …MKC.31547502 Alchemilla semiluna…        NA  63.91587  30.07715
      9        …MKC.31547570 Alchemilla semiluna…        NA  63.91497  30.07705
      10       …MKC.31547511 Bistorta vivipara (…        NA  63.91587  30.07715
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
      1         Heard  468559    175     
      2   Observation  193431    170     
      3          Seen  118110    205     

# can compute a var from id when there are zero records

    Code
      no_record_compute_id
    Output
      Records downloaded: 0
      Records available: 0
      A data.frame [0 x 1]
      [1] finnish_municipality
      <0 rows> (or 0-length row.names)

