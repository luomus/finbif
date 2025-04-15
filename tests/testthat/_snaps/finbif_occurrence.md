# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 10"                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
       [4] "Records available: 15779 + 728"                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
       [5] "A data.frame [20 x 26]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "        scientific_name abundance lat_wgs84 lon_wgs84           date_time"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [7] "1  Falco tinnunculus L…  1         67.10164  23.54135 2023-01-01 11:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [8] "2  Parus major Linnaeu…  6         60.9918   26.16538 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [9] "3  Parus major Linnaeu…  1         64.0057   23.62257 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [10] "4  Certhia familiaris …  1         60.05509  23.67509 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [11] "5  Calidris temminckii…  4         69.16414  21.00689 2023-01-01 11:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [12] "6  Accipiter gentilis …  1         62.14014  24.59975 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [13] "7  Hydrocoloeus minutu…  1         61.49755  23.89792 2023-05-01 05:50:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [14] "8  Pandion haliaetus (…  3         63.80548  22.83368 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [15] "9  Haliaeetus albicill…  1         60.70598  20.58044 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [16] "10 Turdus merula Linna…  1         60.25715  24.73896 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
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
      Records downloaded: 55
      Records available: 55
      A data.frame [55 x 2]
         threatened_status orig_taxon_rank
      1               <NA>         species
      2               <NA>         species
      3               <NA>    nothospecies
      4               <NA>           genus
      5               <NA>           genus
      6               <NA>         species
      7               <NA>           genus
      8               <NA>         species
      9    Near Threatened         species
      10              <NA>           genus
      ...with 45 more records

---

    Code
      occ_print
    Output
      Records downloaded: 11
      Records available: 3310347
      A data.frame [11 x 1]
         informal_groups
      1       3 elements
      2        1 element
      3       2 elements
      4       2 elements
      5       2 elements
      6       2 elements
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
      Records available: 54049 + 35725 + 61992 + 5 + 61992 + 61992 + 10754
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

# can compute a var from id when there are zero records

    Code
      no_record_compute_id
    Output
      Records downloaded: 0
      Records available: 0
      A data.frame [0 x 1]
      [1] finnish_municipality
      <0 rows> (or 0-length row.names)

