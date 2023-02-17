source("data-raw/utils.R")
n <- 1000L
bio_province <- finbif:::api_get(
  list(
    path = "areas",
    query = list(
      type = "biogeographicalProvince", lang = "multi", page = 1L, pageSize = n
    ),
    cache = FALSE
  )
)
stopifnot(n > bio_province[["content"]][["total"]])
bio_province <- bio_province[["content"]][["results"]]
bio_province <- lapply(bio_province, as.data.frame, stringsAsFactors = FALSE)
bio_province <- reduce_merge(bio_province)
bio_province[["country"]] <- finbif:::country_df[
  bio_province[["isPartOf"]], "name_en"
]
bio_province[["name.en"]] <- ifelse(
  nchar(bio_province[["name.en"]]),
  bio_province[["name.en"]],
  bio_province[["name.fi"]]
)
row.names(bio_province) <- bio_province[["id"]]
bio_province["ML.252", "name.en"] <- "Southwest Finland"
bio_province <- bio_province[
  order(bio_province[["name.en"]]),
  c("name.en", "name.fi", "provinceCodeAlpha.fi", "country")
  ]
names(bio_province) <- c("name_en", "name_fi", "alpha", "country")
class(bio_province[["name_en"]]) <- "translation"
class(bio_province[["name_fi"]]) <- "translation"
class(bio_province[["alpha"]]) <- "translation"
