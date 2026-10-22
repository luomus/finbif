# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 10"                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
       [4] "Records available: 16262 + 728"                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
       [5] "A data.frame [20 x 29]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "         scientificName individualCount decimalLatitude decimalLongitude"                                                                                                                                                                                                                                                                                                                                                                                                                              
       [7] "1  Pandion haliaetus (…  1               61.39403        23.346253      "                                                                                                                                                                                                                                                                                                                                                                                                                              
       [8] "2  Haliaeetus albicill…  1               61.064233       24.681628      "                                                                                                                                                                                                                                                                                                                                                                                                                              
       [9] "3  Ficedula hypoleuca …  7               60.991805       26.165382      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [10] "4  Pandion haliaetus (…  2               60.985286       28.567181      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [11] "5  Parus major Linnaeu…  6               60.991805       26.165382      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [12] "6  Strix aluco Linnaeu…  2               61.412064       24.093803      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [13] "7  Pandion haliaetus (…  1               62.337667       26.128257      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [14] "8  Ficedula hypoleuca …  2               68.401824       23.465784      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [15] "9  Pandion haliaetus (…  1                           NA               NA"                                                                                                                                                                                                                                                                                                                                                                                                                              
      [16] "10 Buteo buteo (Linnae…  1               61.75392        27.943401      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [17] "...with 10 more records and 25 more variables:"                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      [18] "eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,"                                                                                                                                                                                                                                                                                                                                                                                                                        
      [19] "requiresIdentification, occurrenceReliability,"                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      [20] "identificationVerificationStatus, samplingEffort, eventDate, datasetName,"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [21] "primaryHabitat, geodeticDatum, occurrenceStatus, bibliographicCitation,"                                                                                                                                                                                                                                                                                                                                                                                                                               
      [22] "collectionCode, redListStatus, stateProvince, informalTaxonGroups,"                                                                                                                                                                                                                                                                                                                                                                                                                                    
      [23] "vernacularName, informationWithheld, dataGeneralizations, atlasCode,"                                                                                                                                                                                                                                                                                                                                                                                                                                  
      [24] "atlasClass, country, modified"                                                                                                                                                                                                                                                                                                                                                                                                                                                                         

---

    Code
      hr778
    Output
      Records downloaded: 1
      Records available: 1
      A data.frame [1 x 12]
                        occurrenceID       scientificName individualCount
      1 http://mus.utu.fi/MY.2475121 Batrachospermum tur…              NA
      ...with 0 more records and 9 more variables:
      decimalLatitude, decimalLongitude, eventDateTime,
      coordinateUncertaintyInMeters, hasIssues, requiresVerification,
      requiresIdentification, occurrenceReliability,
      identificationVerificationStatus

---

    Code
      hr778_no_records
    Output
      Records downloaded: 0 + 0
      Records available: 0 + 0
      A data.frame [0 x 3]
      [1] county       municipality set         
      <0 rows> (or 0-length row.names)

---

    Code
      plants
    Output
      Records downloaded: 32
      Records available: 32
      A data.frame [32 x 3]
         threatenedStatus originalTaxonRank materialEntityType
      1              <NA>           species                 NA
      2              <NA>           species                 NA
      3              <NA>           species                 NA
      4         Statutory        subspecies                 NA
      5              <NA>           species                 NA
      6        Threatened           species                 NA
      7              <NA>             genus                 NA
      8              <NA>             genus                 NA
      9              <NA>           species                 NA
      10             <NA>           species                 NA
      ...with 22 more records

---

    Code
      occ_print
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [2] "Records downloaded: 11"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [3] "Records available: 3392472"                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
       [4] "A data.frame [11 x 1]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
       [5] "   informalTaxonGroups"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "1           2 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [7] "2           4 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [8] "3           4 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [9] "4           4 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
      [10] "5           4 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
      [11] "6           4 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
      [12] "7           4 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
      [13] "8           4 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
      [14] "9           4 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
      [15] "10          4 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
      [16] "...with 1 more record"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 

# fetching occurrences with date filters works

    Code
      date_filters
    Output
      Records downloaded: 5 + 5 + 5 + 5 + 5 + 5 + 5
      Records available: 61564 + 41276 + 69502 + 8 + 69502 + 69502 + 11415
      A data.frame [18 x 12]
                        occurrenceID       scientificName individualCount
      1  …id.herb.oulu.fi/MY.1962140 Tubulicrinis caloth…              NA
      2  …id.herb.oulu.fi/MY.1962197 Skeletocutis papyra…              NA
      3  …id.herb.oulu.fi/MY.1962210 Dacryobolus sudans …              NA
      4  …id.herb.oulu.fi/MY.1962214 Botryobasidium subc…              NA
      5  …id.herb.oulu.fi/MY.1962224 Phlebia segregata (…              NA
      6  …id.herb.oulu.fi/MY.1962230 Jaapia ochroleuca (…              NA
      7  …id.herb.oulu.fi/MY.1962232 Serpula himantioide…              NA
      8  …id.herb.oulu.fi/MY.1962256 Anthoporia albobrun…              NA
      9      …tun.fi/HR.4113/A.322_U Strix aluco Linnaeu…  1             
      10     …tun.fi/HR.4113/A.349_U Strix aluco Linnaeu…  1             
      ...with 8 more records and 9 more variables:
      decimalLatitude, decimalLongitude, eventDateTime,
      coordinateUncertaintyInMeters, hasIssues, requiresVerification,
      requiresIdentification, occurrenceReliability,
      identificationVerificationStatus

# fetching aggregated occurrences works

    Code
      record_basis_aggregate
    Output
      Records downloaded: 2
      Records available: 2
      A data.frame [2 x 3]
        basisOfRecord n_records n_species
      1         Heard  10        3       
      2          Seen  2         1       

# can compute a var from id when there are zero records

    Code
      no_record_compute_id
    Output
      Records downloaded: 0
      Records available: 0
      A data.frame [0 x 1]
      [1] finnishCounty
      <0 rows> (or 0-length row.names)

