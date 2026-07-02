# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 10"                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
       [4] "Records available: 1301538 + 31176"                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
       [5] "A data.frame [20 x 27]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "         scientificName individualCount decimalLatitude decimalLongitude"                                                                                                                                                                                                                                                                                                                                                                                                                              
       [7] "1  Loxia curvirostra L…              NA  63.954953       27.302562      "                                                                                                                                                                                                                                                                                                                                                                                                                              
       [8] "2  Anser anser (Linnae…              NA  64.841425       25.20503       "                                                                                                                                                                                                                                                                                                                                                                                                                              
       [9] "3  Branta canadensis (…              NA  60.451759       21.997063      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [10] "4  Carpodacus erythrin…              NA  62.988568       21.201019      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [11] "5  Bucephala clangula …              NA  64.583134       26.892057      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [12] "6  Passer montanus (Li…              NA  62.340106       27.286209      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [13] "7  Dendrocopos major (…  1               60.438038       22.372606      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [14] "8  Pyrrhula pyrrhula (…  1               59.934902       24.313583      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [15] "9  Poecile montanus (C…  1               60.831401       24.238666      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [16] "10 Sylvia atricapilla …  1               60.176437       24.571127      "                                                                                                                                                                                                                                                                                                                                                                                                                              
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
      Records downloaded: 5
      Records available: 5
      A data.frame [5 x 12]
        occurrenceID       scientificName individualCount decimalLatitude
      1    …10888538 Ceramium tenuicorne…              NA  58.791178     
      2     …2475121 Batrachospermum tur…              NA  60.508382     
      3     …2475345 Lemanea fluviatilis…              NA  60.319239     
      4     …2475348 Furcellaria lumbric…              NA  60.508382     
      5     …2475351 Batrachospermum gel…              NA  60.508382     
      ...with 0 more records and 8 more variables:
      decimalLongitude, eventDateTime, coordinateUncertaintyInMeters, hasIssues,
      requiresVerification, requiresIdentification, occurrenceReliability,
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
      Records downloaded: 21
      Records available: 21
      A data.frame [21 x 3]
         threatenedStatus originalTaxonRank materialEntityType
      1                NA           species                 NA
      2                NA           species                 NA
      3                NA           species                 NA
      4                NA           species                 NA
      5                NA             genus                 NA
      6                NA           species                 NA
      7                NA        subspecies                 NA
      8                NA           species                 NA
      9                NA           species                 NA
      10               NA           species                 NA
      ...with 11 more records

---

    Code
      occ_print
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [2] "Records downloaded: 11"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [3] "Records available: 60339254"                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
       [4] "A data.frame [11 x 1]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
       [5] "   informalTaxonGroups"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "1           2 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [7] "2           2 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [8] "3           2 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [9] "4           2 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
      [10] "5           2 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
      [11] "6           2 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
      [12] "7           2 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
      [13] "8           2 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
      [14] "9           2 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
      [15] "10          2 elements"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
      [16] "...with 1 more record"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 

# fetching occurrences with date filters works

    Code
      date_filters
    Output
      Records downloaded: 5 + 5 + 5 + 5 + 5 + 5 + 5
      Records available: 1531613 + 1147808 + 1676085 + 900 + 1676085 + 1676085 + 13…
      A data.frame [15 x 12]
                      occurrenceID       scientificName individualCount
      1  …herb.oulu.fi/MY.10079449 Polytrichum juniper…              NA
      2  …herb.oulu.fi/MY.10079491 Polytrichum pilifer…              NA
      3  …herb.oulu.fi/MY.10079494   Cladonia P. Browne              NA
      4  …herb.oulu.fi/MY.10079521 Polytrichum strictu…              NA
      5  …herb.oulu.fi/MY.10079581 Polytrichum jenseni…              NA
      6   …herb.oulu.fi/MY.4185479 Trametes betulina (…              NA
      7     …luomus.fi/MY.13883497 Phanerochaete lives…              NA
      8     …luomus.fi/MY.15939503 Resinicium bicolor …              NA
      9     …luomus.fi/MY.16033749 Skeletocutis amorph…              NA
      10    …luomus.fi/MY.16286825 Myotis brandtii (Ev…              NA
      ...with 5 more records and 9 more variables:
      decimalLatitude, decimalLongitude, eventDateTime,
      coordinateUncertaintyInMeters, hasIssues, requiresVerification,
      requiresIdentification, occurrenceReliability,
      identificationVerificationStatus

# fetching aggregated occurrences works

    Code
      record_basis_aggregate
    Output
      Records downloaded: 3
      Records available: 3
      A data.frame [3 x 3]
        basisOfRecord n_records n_species
      1         Heard  491003    174     
      2   Observation  196341    169     
      3          Seen  123599    203     

# can compute a var from id when there are zero records

    Code
      no_record_compute_id
    Output
      Records downloaded: 0
      Records available: 0
      A data.frame [0 x 1]
      [1] finnishCounty
      <0 rows> (or 0-length row.names)

