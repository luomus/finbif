source("data-raw/utils.R")
n <- 1000L
province <- finbif:::api_get(
  "areas",
  list(
    type = "biogeographicalProvince", lang = "multi", page = 1L, pageSize = n
  ),
  FALSE
)
stopifnot(n > province[["content"]][["total"]])
province <- province[["content"]][["results"]]
province <- lapply(province, as.data.frame, stringsAsFactors = FALSE)
province <- reduce_merge(province)
province[["country"]] <- finbif:::country[province[["isPartOf"]], "name_en"]
province[["name.en"]] <- ifelse(
  nchar(province[["name.en"]]), province[["name.en"]], province[["name.fi"]]
)
row.names(province) <- province[["id"]]
province["ML.252", "name.en"] <- "Southwest Finland"
province <- province[
  order(province[["name.en"]]),
  c("name.en", "name.fi", "provinceCodeAlpha.fi", "country")
  ]
names(province) <- c("name_en", "name_fi", "alpha", "country")
class(province[["name_en"]]) <- "translation"
class(province[["name_fi"]]) <- "translation"
class(province[["alpha"]]) <- "translation"
