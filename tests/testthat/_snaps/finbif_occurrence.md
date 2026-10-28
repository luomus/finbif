# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 10"                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
       [4] "Records available: 16262 + 728"                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
       [5] "A data.frame [20 x 26]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "         scientificName individualCount decimalLatitude decimalLongitude"                                                                                                                                                                                                                                                                                                                                                                                                                              
       [7] "1  Ficedula hypoleuca …  6               60.90207        26.167729      "                                                                                                                                                                                                                                                                                                                                                                                                                              
       [8] "2  Glaucidium passerin…  1               62.319435       24.585453      "                                                                                                                                                                                                                                                                                                                                                                                                                              
       [9] "3  Ficedula hypoleuca …  4               60.541874       25.994788      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [10] "4  Muscicapa striata (…  3               60.453381       26.179239      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [11] "5  Caprimulgus europae…  1               60.354983       25.275819      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [12] "6  Ficedula hypoleuca …  2               60.90207        26.167729      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [13] "7  Ficedula hypoleuca …  6               60.903084       26.351967      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [14] "8  Luscinia luscinia (…  1               61.481973       23.825045      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [15] "9  Accipiter nisus (Li…  2               60.325376       21.109411      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [16] "10 Parus major Linnaeu…  8               68.401824       23.465784      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [17] "...with 10 more records and 22 more variables:"                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      [18] "coordinateUncertaintyInMeters, hasIssues, requiresVerification,"                                                                                                                                                                                                                                                                                                                                                                                                                                       
      [19] "requiresIdentification, occurrenceReliability,"                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      [20] "identificationVerificationStatus, eventDate, eventDate, datasetName,"                                                                                                                                                                                                                                                                                                                                                                                                                                  
      [21] "primaryHabitat, geodeticDatum, occurrenceStatus, bibliographicCitation,"                                                                                                                                                                                                                                                                                                                                                                                                                               
      [22] "collectionCode, redListStatus, stateProvince, informalTaxonGroups,"                                                                                                                                                                                                                                                                                                                                                                                                                                    
      [23] "vernacularName, informationWithheld, dataGeneralizations, country, modified"                                                                                                                                                                                                                                                                                                                                                                                                                           

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
      decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, hasIssues,
      requiresVerification, requiresIdentification, occurrenceReliability,
      identificationVerificationStatus, eventDate

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
      1        Threatened           species  PreservedSpecimen
      2              <NA>           species               <NA>
      3              <NA>           species               <NA>
      4              <NA>           species               <NA>
      5              <NA>           species               <NA>
      6              <NA>             genus               <NA>
      7              <NA>             genus               <NA>
      8              <NA>             genus               <NA>
      9              <NA>             genus               <NA>
      10             <NA>             genus               <NA>
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
      decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, hasIssues,
      requiresVerification, requiresIdentification, occurrenceReliability,
      identificationVerificationStatus, eventDate

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

