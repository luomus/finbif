# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 10"                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
       [4] "Records available: 16262 + 728"                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
       [5] "A data.frame [20 x 27]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "         scientificName individualCount decimalLatitude decimalLongitude"                                                                                                                                                                                                                                                                                                                                                                                                                              
       [7] "1  Strix uralensis Pal…  1               62.332835       25.549466      "                                                                                                                                                                                                                                                                                                                                                                                                                              
       [8] "2  Pandion haliaetus (…  2               61.350772       27.837802      "                                                                                                                                                                                                                                                                                                                                                                                                                              
       [9] "3  Buteo buteo (Linnae…  2               62.401823       24.191793      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [10] "4  Falco tinnunculus L…  1               62.569702       22.131128      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [11] "5  Ficedula hypoleuca …  4               63.374228       30.496256      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [12] "6  Falco tinnunculus L…  1               62.569702       22.131128      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [13] "7  Haliaeetus albicill…  1               63.463529       21.979449      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [14] "8  Anas crecca Linnaeu…  6               63.305567       29.489949      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [15] "9  Parus major Linnaeu…  3               60.90207        26.167729      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [16] "10 Parus major Linnaeu…  1               60.90207        26.167729      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [17] "...with 10 more records and 23 more variables:"                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      [18] "eventDateTime, coordinateUncertaintyInMeters, hasIssues, requiresVerification,"                                                                                                                                                                                                                                                                                                                                                                                                                        
      [19] "requiresIdentification, occurrenceReliability,"                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      [20] "identificationVerificationStatus, samplingEffort, eventDate, datasetName,"                                                                                                                                                                                                                                                                                                                                                                                                                             
      [21] "primaryHabitat, geodeticDatum, occurrenceStatus, bibliographicCitation,"                                                                                                                                                                                                                                                                                                                                                                                                                               
      [22] "collectionCode, redListStatus, stateProvince, informalTaxonGroups,"                                                                                                                                                                                                                                                                                                                                                                                                                                    
      [23] "vernacularName, informationWithheld, atlasCode, atlasClass, country"                                                                                                                                                                                                                                                                                                                                                                                                                                   

---

    Code
      hr778
    Output
      Records downloaded: 1
      Records available: 1
      A data.frame [1 x 12]
                        occurrenceID       scientificName individualCount
      1 http://mus.utu.fi/MY.2475121 Batrachospermum tur…              NA
      ...with 0 more record and 9 more variables:
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
      Records downloaded: 48
      Records available: 48
      A data.frame [48 x 2]
         threatenedStatus originalTaxonRank
      1              <NA>           species
      2              <NA>           species
      3         Statutory           species
      4              <NA>             genus
      5         Statutory           species
      6              <NA>           species
      7              <NA>           species
      8              <NA>             genus
      9              <NA>           species
      10        Statutory           species
      ...with 38 more records

---

    Code
      occ_print
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [2] "Records downloaded: 11"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [3] "Records available: 3374409"                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
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
      Records available: 60224 + 40282 + 68160 + 8 + 68160 + 68160 + 11418
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
      1         Heard  7         3       
      2          Seen  1         1       

# can compute a var from id when there are zero records

    Code
      no_record_compute_id
    Output
      Records downloaded: 0
      Records available: 0
      A data.frame [0 x 1]
      [1] finnishCounty
      <0 rows> (or 0-length row.names)

