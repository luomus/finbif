source("data-raw/utils.R")

filter_names <- read.csv(
  "data-raw/filters.csv", stringsAsFactors = FALSE, strip.white = TRUE,
  row.names = 1L
)

req <- httr2::request("https://laji.fi/api/warehouse/filters")

if (identical(Sys.getenv("BRANCH"), "dev")) {
  req <- httr2::request("https://dev.laji.fi/api/warehouse/filters")
}

filters <- names(httr2::resp_body_json(httr2::req_perform(req)))

stopifnot(
  identical(
    sort(c(filters, "excludeNulls")),
    sort(row.names(filter_names))
  ) ||
    identical(
      sort(filter_names[filter_names[["doc"]], "translated_filter"]),
      sort(documented_vars("R/filters.R"))
    )
)

unused_filters <- c("editorId", "editorOrObserverId", "observerId")

filter_names <- filter_names[-match(unused_filters, rownames(filter_names)), ]

class(filter_names[["translated_filter"]]) <- "translation"

filter_names_df <- filter_names
