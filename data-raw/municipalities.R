source("data-raw/utils.R")
n <- 1000L
municipality <- finbif:::api_get(
  list(
    path = "areas",
    query = list(
      type = "municipality", lang = "multi", page = 1L, pageSize = n
    ),
    cache = FALSE
  )
)
stopifnot(n > municipality[["content"]][["total"]])
municipality <- municipality[["content"]][["results"]]
municipality <-
  lapply(municipality, as.data.frame, stringsAsFactors = FALSE)
municipality <- reduce_merge(municipality)
municipality[["country"]] <-
  finbif:::country_df[municipality[["isPartOf"]], "name_en"]
municipality[["name.en"]] <- ifelse(
  nchar(municipality[["name.en"]]), municipality[["name.en"]],
  municipality[["name.fi"]]
)
row.names(municipality) <- municipality[["id"]]
municipality <- municipality[
  order(municipality[["name.en"]]), c("name.en", "name.fi", "country")
]
names(municipality) <- c("name_en", "name_fi", "country")
class(municipality[["name_en"]]) <- "translation"
class(municipality[["name_fi"]]) <- "translation"

file <- tempfile()
geo_table <- download.file(
  file.path(
    "https://raw.githubusercontent.com",
    "rOpenGov/geofi/master/data/municipality_key_2022.rda"
  ),
  destfile = file, quiet = TRUE
)
load(file)

municipality[["region"]] <- as.data.frame(municipality_key_2022)[
  order(municipality_key_2022[["municipality_name_fi"]]),
  "maakunta_name_fi"
]

# english names are currently redundant
municipality[["name_en"]] <- NULL
