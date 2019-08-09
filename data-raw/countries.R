countries <- finbif:::finbif_api_get(
  "v0/areas",
  list(type = "country", lang = "multi", page = 1L, pageSize = 1000L),
  FALSE
)
countries <- countries[["content"]][["results"]]
countries <- lapply(countries, as.data.frame, stringsAsFactors = FALSE)
countries <- finbif:::reduce_merge(countries)
countries <- merge(
  countries, ISOcodes::ISO_3166_1, by.x = "countryCodeISOalpha2",
  by.y = "Alpha_2", all.x = TRUE
)
countries[["name.en"]] <- ifelse(
  is.na(countries[["Common_name"]]),
  ifelse(
    is.na(countries[["Name"]]), countries[["name.en"]], countries[["Name"]]
  ),
  countries[["Common_name"]]
)
row.names(countries) <- countries[["id"]]
countries <- countries[
  order(countries["name.en"]),
  c("name.en", "name.fi", "countryCodeISOalpha2", "countryCodeISOalpha3")
]
names(countries) <- c("name_en", "name_fi", "alpha2", "alpha3")
