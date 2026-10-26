# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 10"                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
       [4] "Records available: 16262 + 728"                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
       [5] "A data.frame [20 x 28]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "         scientificName individualCount decimalLatitude decimalLongitude"                                                                                                                                                                                                                                                                                                                                                                                                                              
       [7] "1  Falco tinnunculus L…  1               64.22146        27.923485      "                                                                                                                                                                                                                                                                                                                                                                                                                              
       [8] "2  Astur gentilis (Lin…  1               62.158235       26.133424      "                                                                                                                                                                                                                                                                                                                                                                                                                              
       [9] "3  Haliaeetus albicill…  1               68.232341       23.97563       "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [10] "4  Pandion haliaetus (…  2                           NA               NA"                                                                                                                                                                                                                                                                                                                                                                                                                              
      [11] "5  Turdus pilaris Linn…  1               62.245009       25.746111      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [12] "6  Pandion haliaetus (…  2                           NA               NA"                                                                                                                                                                                                                                                                                                                                                                                                                              
      [13] "7  Falco tinnunculus L…  2               62.069615       27.666235      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [14] "8  Turdus iliacus Linn…  5               60.637305       22.151356      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [15] "9  Phoenicurus phoenic…  5               67.792702       29.484887      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [16] "10 Aythya fuligula (Li…  3               61.497546       23.897924      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [17] "...with 10 more records and 24 more variables:"                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      [18] "coordinateUncertaintyInMeters, hasIssues, requiresVerification,"                                                                                                                                                                                                                                                                                                                                                                                                                                       
      [19] "requiresIdentification, occurrenceReliability,"                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      [20] "identificationVerificationStatus, eventDate, eventDate, datasetName,"                                                                                                                                                                                                                                                                                                                                                                                                                                  
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
      1                NA             genus                 NA
      2                NA           species                 NA
      3                NA             genus                 NA
      4                NA             genus                 NA
      5                NA             genus                 NA
      6                NA             genus                 NA
      7                NA             genus                 NA
      8                NA             genus                 NA
      9                NA             genus                 NA
      10               NA             genus                 NA
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

