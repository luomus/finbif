n <- 1000L
provinces <- finbif:::finbif_api_get(
  "v0/areas",
  list(
    type = "biogeographicalProvince", lang = "multi", page = 1L, pageSize = n
  ),
  FALSE
)
stopifnot(n > provinces[["content"]][["total"]])
provinces <- provinces[["content"]][["results"]]
provinces <- lapply(provinces, as.data.frame, stringsAsFactors = FALSE)
provinces <- finbif:::reduce_merge(provinces)
provinces[["country"]] <- finbif:::countries[provinces[["isPartOf"]], "name_en"]
provinces[["name.en"]] <- ifelse(
  nchar(provinces[["name.en"]]), provinces[["name.en"]], provinces[["name.fi"]]
)
row.names(provinces) <- provinces[["id"]]
provinces["ML.252", "name.en"] <- "Southwest Finland"
provinces <- provinces[
  order(provinces[["name.en"]]),
  c("name.en", "name.fi", "provinceCodeAlpha.fi", "country")
  ]
names(provinces) <- c("name_en", "name_fi", "alpha", "country")
