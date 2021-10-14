source("data-raw/utils.R")
filter_names_test <- read.csv(
  "data-raw/filters_test.csv", stringsAsFactors = FALSE, strip.white = TRUE,
  row.names = 1L
)

token <- Sys.getenv("FINBIF_ACCESS_TOKEN")
Sys.setenv(FINBIF_ACCESS_TOKEN = Sys.getenv("FINBIF_DEV_ACCESS_TOKEN"))

options(finbif_api_url = "https://apitest.laji.fi")

filters <- names(
  finbif:::api_get("warehouse/filters", list(), FALSE)[["content"]]
)

stopifnot(
  identical(
    sort(filters),
    sort(row.names(filter_names_test))
  )
)

stopifnot(
  identical(
    sort(filter_names_test[filter_names_test[["doc"]], "translated_filter"]),
    sort(documented_vars("R/filters.R"))
  )
)

# Some filter have been deprecated or are not available in the public API
unused_filters <- c("editorId", "editorOrObserverId", "observerId")

filter_names_test <- filter_names_test[
  -match(unused_filters, rownames(filter_names_test)),
]

class(filter_names_test[["translated_filter"]]) <- "translation"

options(finbif_api_url = "https://api.laji.fi")

Sys.setenv(FINBIF_ACCESS_TOKEN = token)

rm(token)
