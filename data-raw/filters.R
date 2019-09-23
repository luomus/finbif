filter_names <- read.csv(
  "data-raw/filters.csv", stringsAsFactors = FALSE, strip.white = TRUE,
  row.names = 1L
)

filters <- names(
  finbif:::finbif_api_get("warehouse/filters", list(), FALSE)[["content"]]
)

stopifnot(identical(sort(row.names(filter_names)), sort(filters)))

# Some filter have been deprecated
filter_names <- filter_names[-match("namedPlaceId", rownames(filter_names)), ]

class(filter_names[["translated_filter"]]) <- "translation"
