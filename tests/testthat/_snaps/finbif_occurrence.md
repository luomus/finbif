# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 10"                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
       [4] "Records available: 1286040 + 31112"                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
       [5] "A data.frame [20 x 26]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "        scientific_name abundance lat_wgs84 lon_wgs84           date_time"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [7] "1  Anser fabalis (Lath…  1         65.82749  25.13652 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [8] "2  Carduelis spinus (L…        NA  62.22949  25.74921 2023-05-03 07:01:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
       [9] "3  Falco tinnunculus L…        NA  64.66836  25.84476 2023-06-13 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [10] "4  Cuculus canorus Lin…        NA  62.76764  24.54885 2023-05-20 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [11] "5  Parus major Linnaeu…        NA  60.04046  23.13774 2023-06-10 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [12] "6  Corvus corax Linnae…  2         60.14558  24.88929 2023-04-02 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [13] "7  Dendrocopos major (…        NA  61.48712  23.79722 2023-05-31 01:02:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [14] "8  Carduelis chloris (…        NA  61.29396  22.98437 2023-05-22 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [15] "9  Numenius arquata (L…  2         60.1895   24.8191  2023-06-30 09:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [16] "10 Regulus regulus (Li…  1         60.8314   24.23867 2023-01-01 10:00:00"                                                                                                                                                                                                                                                                                                                                                                                                                             
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
      Records downloaded: 52
      Records available: 52
      A data.frame [52 x 2]
         threatened_status orig_taxon_rank
      1                 NA         species
      2                 NA           genus
      3                 NA         species
      4                 NA         species
      5                 NA         species
      6                 NA         species
      7                 NA         species
      8                 NA         species
      9                 NA         species
      10                NA         species
      ...with 42 more records

---

    Code
      occ_print
    Output
      Records downloaded: 11
      Records available: 54814965
      A data.frame [11 x 1]
         informal_groups
      1        1 element
      2        1 element
      3       3 elements
      4       3 elements
      5       3 elements
      6       4 elements
      7       4 elements
      8       3 elements
      9       4 elements
      10      2 elements
      ...with 1 more record

# fetching occurrences with date filters works

    Code
      date_filters
    Output
      Records downloaded: 5 + 5 + 5 + 5 + 5 + 5 + 5
      Records available: 1508213 + 1130947 + 1650249 + 902 + 1650249 + 1650249 + 13…
      A data.frame [20 x 12]
                          record_id      scientific_name abundance lat_wgs84
      1  …tun.fi/KE.67/9586110#Unit Larus ridibundus (L…  1         53.21667
      2  …tun.fi/KE.67/9586592#Unit Larus ridibundus (L…  1         53.2    
      3  …tun.fi/KE.67/9590259#Unit Larus ridibundus (L…  1         56.15   
      4  …tun.fi/KE.67/9590262#Unit Larus ridibundus (L…  1         56.16667
      5  …tun.fi/KE.67/9605091#Unit Larus ridibundus (L…  1         52.1    
      6   …id.luomus.fi/MY.18131877 Nola aerugula (Hübn…  1         60.256  
      7   …id.luomus.fi/MY.18131885 Nola aerugula (Hübn…  1         60.256  
      8   …id.luomus.fi/MY.18131893 Nola aerugula (Hübn…  1         60.256  
      9   …id.luomus.fi/MY.18131901 Nola aerugula (Hübn…  1         60.256  
      10  …id.luomus.fi/MY.18131905 Nola aerugula (Hübn…  1         60.256  
         lon_wgs84
      1   6.566667
      2   5.783333
      3   10.21667
      4   10.21667
      5   5.116667
      6   25.141  
      7   25.141  
      8   25.141  
      9   25.141  
      10  25.141  
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
      1         Heard  445364    173     
      2   Observation  191156    169     
      3          Seen  113056    204     

# can compute a var from id when there are zero records

    Code
      no_record_compute_id
    Output
      Records downloaded: 0
      Records available: 0
      A data.frame [0 x 1]
      [1] finnish_municipality
      <0 rows> (or 0-length row.names)

