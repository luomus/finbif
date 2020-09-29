finnish_occurrence_status <- read.csv(text = "
  id,                                                               desc,                                                                                         code
  MX.doesNotOccur,                                                  Not considered to occur in Finland,                                                           none
  MX.typeOfOccurrenceOccurs,                                        Occurs in Finland,                                                                            occurs
  MX.typeOfOccurrenceStablePopulation,                              Finnish population stable,                                                                    stable
  MX.typeOfOccurrenceCommon,                                        Common in Finland,                                                                            common
  MX.typeOfOccurrenceRare,                                          Rare in Finland,                                                                              rare
  MX.typeOfOccurrenceVeryRare,                                      Very rare in Finland,                                                                         very_rare
  MX.typeOfOccurrenceVagrant,                                       Regular vagrant to Finland,                                                                   vagrant_regular
  MX.typeOfOccurrenceRareVagrant,                                   Irregular vagrant to Finland,                                                                 vagrant_irregular
  MX.typeOfOccurrenceMigrant,                                       Finnish migrant taxa,                                                                         migrant
  MX.typeOfOccurrenceImport,                                        Imported to Finland via trade,                                                                import
  MX.typeOfOccurrenceAnthropogenic,                                 Deliberately introduced to Finland,                                                           introduced
  MX.typeOfOccurrenceNotEstablished,                                Not established in Finland,                                                                   unestablished
  MX.typeOfOccurrenceExtirpated,                                    Extinct in Finland,                                                                           extinct
  MX.typeOfOccurrenceOldRecords,                                    Known only from historic records in Finland,                                                  historic
  MX.typeOfOccurrenceUncertain,                                     Known only from uncertain records in Finland,                                                 uncertain
  MX.typeOfOccurrenceMaxReplanted,                                  Reintroduced to Finland,                                                                      reintroduced
  MX.typeOfOccurrenceNotEvaluated,                                  Not evaluated,                                                                                unevaluated
  MX.typeOfOccurrenceSpontaneousOldResident,                        'Spontaneously arrived, old Finnish resident',                                                SOR
  MX.typeOfOccurrenceSpontaneousOldFormerlyResidentPossiblyExtinct, 'Spontaneously arrived, old Finnish resident now possibly extinct',                           SORPE
  MX.typeOfOccurrenceSpontaneousOldFormerlyResidentExtinct,         'Spontaneously arrived, old Finnish resident now extinct',                                    SORE
  MX.typeOfOccurrenceSpontaneousNewResident,                        'Spontaneously arrived, new Finnish resident',                                                SNR
  MX.typeOfOccurrenceSpontaneousNewEphemeral,                       'Spontaneously arrived, new Finnish ephemeral',                                               SNE
  MX.typeOfOccurrenceSpontaneousNewEphemeralOnlyOld,                'Spontaneously arrived, new Finnish ephemeral known only from historic records',              SNEH
  MX.typeOfOccurrenceAlienOldResident,                              'Alien, old Finnish resident (archaeophyte)',                                                 AOR
  MX.typeOfOccurrenceAlienOldFormerlyResidentPossiblyExtinct,       'Alien, old Finnish resident now possibly extinct',                                           AORPE
  MX.typeOfOccurrenceAlienOldExtinct,                               'Alien, old Finnish resident now extinct',                                                    AORE
  MX.typeOfOccurrenceAlienNewResident,                              'Alien, new Finnish resident',                                                                ANR
  MX.typeOfOccurrenceAlienNewEphemeral,                             'Alien, new Finnish ephemeral',                                                               ANE
  MX.typeOfOccurrenceAlienNewEphemeralOnlyold,                      'Alien, new Finnish ephemeral known only from historic records (pre 1979)',                   ANEH
  MX.typeOfOccurrenceCompletelyCultivatedOrigin,                    All of Finnish wild population escaped from cultivation,                                      cultivation_escape_all
  MX.typeOfOccurrenceNotableDegreeCultivatedOrigin,                 Much of Finnish wild population escaped from cultivation,                                     cultivation_escape_much
  MX.typeOfOccurrenceMaxShortDistanceEscape,                        'In Finland, at most a short-distance escapee from cultivation (inside a garden or similar)', cultivation_escape_short
  MX.typeOfOccurrenceSmallDegreeCultivatedOrigin,                   Some of Finnish wild population escaped from cultivation,                                     cultivation_escape_some
  MX.typeOfOccurrenceOnlyCultivated,                                'In Finland, only in cultivation',                                                            cultivation_only
  MX.typeOfOccurrenceMaxRelict,                                     A relict of cultivation in Finland,                                                           cultivation_relict
  MX.typeOfOccurrenceMaxSoilImmigrant,                              'In Finland, at most a soil immigrant, possibly spreading vegetatively',                      soil_immigrant
  MX.typeOfOccurrenceOccursBasedOnOccurrences,                      'Occurrence status based on occurrence records only and not based an any expert evaluation',  records_only
", stringsAsFactors = FALSE, strip.white = TRUE, row.names = 1L, quote = "'")

class(finnish_occurrence_status[["code"]]) <- "translation"

finnish_occurrence_status_neg <- finnish_occurrence_status

metadata_ranges <-
  finbif:::api_get("metadata/ranges", list(), FALSE)[["content"]]

fos <- metadata_ranges[["MX.typeOfOccurrenceEnum"]]
fos <- lapply(fos, `[`, c("id", "value"))
fos <- matrix(unlist(fos), ncol = 2L, byrow = TRUE)[, 1L]

stopifnot(identical(sort(row.names(finnish_occurrence_status)), sort(fos)))
