source("data-raw/utils.R")
n <- 1000L
municipality <- finbif:::finbif_api_get(
  "areas", list(type = "municipality", lang = "multi", page = 1L, pageSize = n),
  FALSE
)
stopifnot(n > municipality[["content"]][["total"]])
municipality <- municipality[["content"]][["results"]]
municipality <-
  lapply(municipality, as.data.frame, stringsAsFactors = FALSE)
municipality <- reduce_merge(municipality)
municipality[["country"]] <-
  finbif:::country[municipality[["isPartOf"]], "name_en"]
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
