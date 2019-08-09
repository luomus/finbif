n <- 1000L
municipalities <- finbif:::finbif_api_get(
  "areas",
  list(type = "municipality", lang = "multi", page = 1L, pageSize = n),
  FALSE
)
stopifnot(n > municipalities[["content"]][["total"]])
municipalities <- municipalities[["content"]][["results"]]
municipalities <-
  lapply(municipalities, as.data.frame, stringsAsFactors = FALSE)
municipalities <- finbif:::reduce_merge(municipalities)
municipalities[["country"]] <-
  finbif:::countries[municipalities[["isPartOf"]], "name_en"]
municipalities[["name.en"]] <- ifelse(
  nchar(municipalities[["name.en"]]), municipalities[["name.en"]],
  municipalities[["name.fi"]]
)
row.names(municipalities) <- municipalities[["id"]]
municipalities <- municipalities[
  order(municipalities[["name.en"]]), c("name.en", "name.fi", "country")
]
names(municipalities) <- c("name_en", "name_fi", "country")
