source("data-raw/utils.R")
filter_names <- read.csv(
  "data-raw/filters.csv", stringsAsFactors = FALSE, strip.white = TRUE,
  row.names = 1L
)

filters <- names(
  finbif:::api_get("warehouse/filters", list(), FALSE)[["content"]]
)

stopifnot(
  identical(
    sort(filters),
    sort(row.names(filter_names))
  )
)

stopifnot(
  identical(
    sort(filter_names[filter_names[["doc"]], "translated_filter"]),
    sort(documented_vars("R/filters.R"))
  )
)

# Some filter have been deprecated or are not available in the public API
unused_filters <-
  c("namedPlaceId", "editorId", "editorOrObserverId", "observerId")

filter_names <- filter_names[-match(unused_filters, rownames(filter_names)), ]

class(filter_names[["translated_filter"]]) <- "translation"
