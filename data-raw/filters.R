filter_names <- read.csv(
  "data-raw/filters.csv", stringsAsFactors = FALSE, strip.white = TRUE,
  row.names = 1L
)

filters <- names(
  finbif:::finbif_api_get("warehouse/filters", list(), FALSE)[["content"]]
)

stopifnot(identical(sort(row.names(filter_names)), sort(filters)))

# Some filter have been deprecated or are not available in the public API
unused_filters <-
  c("namedPlaceId", "editorId", "editorOrObserverId", "observerId")

filter_names <- filter_names[-match(unused_filters, rownames(filter_names)), ]

class(filter_names[["translated_filter"]]) <- "translation"
