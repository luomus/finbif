source("data-raw/utils.R")
country <- finbif:::api_get(
  "areas",
  list(type = "country", lang = "multi", page = 1L, pageSize = 1000L),
  FALSE
)
country <- country[["content"]][["results"]]
country <- lapply(country, as.data.frame, stringsAsFactors = FALSE)
country <- reduce_merge(country)
country <- merge(
  country, ISOcodes::ISO_3166_1, by.x = "countryCodeISOalpha2",
  by.y = "Alpha_2", all.x = TRUE
)
country[["name.en"]] <- ifelse(
  is.na(country[["Common_name"]]),
  ifelse(
    is.na(country[["Name"]]), country[["name.en"]], country[["Name"]]
  ),
  country[["Common_name"]]
)
row.names(country) <- country[["id"]]
country <- country[
  order(country[["name.en"]]),
  c("name.en", "name.fi", "countryCodeISOalpha2", "countryCodeISOalpha3")
]
names(country) <- c("name_en", "name_fi", "alpha2", "alpha3")
class(country[["name_en"]]) <- "translation"
class(country[["name_fi"]]) <- "translation"
class(country[["alpha2"]]) <- "translation"
class(country[["alpha3"]]) <- "translation"
