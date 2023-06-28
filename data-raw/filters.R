source("data-raw/utils.R")

filter_names <- read.csv(
  "data-raw/filters.csv", stringsAsFactors = FALSE, strip.white = TRUE,
  row.names = 1L
)

token <- Sys.getenv("FINBIF_ACCESS_TOKEN")

if (identical(Sys.getenv("BRANCH"), "dev")) {

  Sys.setenv(FINBIF_ACCESS_TOKEN = Sys.getenv("FINBIF_DEV_ACCESS_TOKEN"))

  options(finbif_api_url = "https://apitest.laji.fi")

}

filters <- try(names(
  finbif:::api_get(
    list(path = "warehouse/filters", query = list(), cache = FALSE)
  )[["content"]]
))

options(finbif_api_url = "https://api.laji.fi")

Sys.setenv(FINBIF_ACCESS_TOKEN = token)

rm(token)

stopifnot(
  identical(
    sort(c(filters, "excludeNulls")),
    sort(row.names(filter_names))
  )
)

stopifnot(
  identical(
    sort(filter_names[filter_names[["doc"]], "translated_filter"]),
    sort(documented_vars("R/filters.R"))
  )
)

unused_filters <- c("editorId", "editorOrObserverId", "observerId")

filter_names <- filter_names[-match(unused_filters, rownames(filter_names)), ]

class(filter_names[["translated_filter"]]) <- "translation"

filter_names_df <- filter_names
