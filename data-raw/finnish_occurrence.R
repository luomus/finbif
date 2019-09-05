finnish_occurrence_status <- read.csv(text = "
  id,                                       desc,                                                   code
  MX.doesNotOccur,                          Not considered to occur in Finland,                     none
  MX.occurrenceInFinlandPublished,          Finnish occurrence in publication,                      publication
  MX.occurrenceInFinlandCollected,          Specimum collected in Finland,                          collection
  MX.occurrenceInFinlandObserved,           Observed in Finland,                                    observation
  MX.occurrenceInFinlandPublishedUncertain, Finnish occurrence in publication (possibly erroneous), publication_uncertain
  MX.occurrenceInFinlandCollectedUncertain, Specimum collected in Finland (possibly erroneous),     collection_uncertain
  MX.occurrenceInFinlandObservedUncertain,  Observed in Finland (possibly erroneous),               observation_uncertain
  MX.occurrenceInFinlandPublishedError,     Finnish occurrence in publication (erroneous),          publication_error
  MX.occurrenceInFinlandPresumed,           Finnish occurrence presumed,                            presumed
", stringsAsFactors = FALSE, strip.white = TRUE, row.names = 1L, quote = "'")

class(finnish_occurrence_status[["code"]]) <- "translation"

finnish_occurrence_status_neg <- finnish_occurrence_status

metadata_ranges <-
  finbif:::finbif_api_get("metadata/ranges", list(), FALSE)[["content"]]

fos <- metadata_ranges[["MX.occurrenceInFinlandEnum"]]
fos <- matrix(unlist(fos), ncol = 2L, byrow = TRUE)[, 1L]

stopifnot(identical(sort(row.names(finnish_occurrence_status)), sort(fos)))
