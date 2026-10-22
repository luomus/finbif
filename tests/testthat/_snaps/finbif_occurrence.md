# fetching occurrences works

    Code
      birds
    Output
       [1] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [2] "\r  |                                                                            \r  |                                                                      |   0%\r  |                                                                            \r  |===================================                                   |  50%\r  |                                                                            \r  |======================================================================| 100%"
       [3] "Records downloaded: 10 + 10"                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
       [4] "Records available: 1300134 + 31176"                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
       [5] "A data.frame [20 x 29]"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
       [6] "         scientificName individualCount decimalLatitude decimalLongitude"                                                                                                                                                                                                                                                                                                                                                                                                                              
       [7] "1  Pica pica (Linnaeus…              NA  60.129259       24.668872      "                                                                                                                                                                                                                                                                                                                                                                                                                              
       [8] "2  Riparia riparia (Li…              NA  62.334713       25.742372      "                                                                                                                                                                                                                                                                                                                                                                                                                              
       [9] "3  Phylloscopus trochi…              NA  61.593456       23.159548      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [10] "4  Cyanistes caeruleus…  1               63.321443       28.293671      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [11] "5  Phylloscopus trochi…              NA  61.96701        28.998692      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [12] "6  Anas crecca Linnaeu…              NA  65.728506       24.489565      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [13] "7  Ardea cinerea Linna…  1               60.173105       25.105165      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [14] "8  Turdus merula Linna…  2               60.439655       22.37459       "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [15] "9  Sterna hirundo Linn…  1               60.776376       23.784256      "                                                                                                                                                                                                                                                                                                                                                                                                                              
      [16] "10 Turdus philomelos C…  1               59.813927       20.753454      "                                                                                                                                                                                                                                                                                                                                                                                                                              
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
      5                NA           species                 NA
      6                NA             genus                 NA
      7                NA           species                 NA
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
       [3] "Records available: 61211261"                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
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
      Records available: 1536548 + 1151557 + 1681099 + 900 + 1681099 + 1681099 + 13…
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
      1         Heard  492560    174     
      2   Observation  196929    169     
      3          Seen  123929    203     

# can compute a var from id when there are zero records

    Code
      no_record_compute_id
    Output
      Records downloaded: 0
      Records available: 0
      A data.frame [0 x 1]
      [1] finnishCounty
      <0 rows> (or 0-length row.names)

