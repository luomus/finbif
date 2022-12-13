metadata_ranges <-
  finbif:::api_get(
    list(path = "metadata/ranges", query = list(), cache = FALSE)
  )[["content"]]

get_habitat_types <- function(x) {

  habitat_types <- metadata_ranges[x][[1]]
  habitat_types <- lapply(habitat_types, head, 1L)
  habitat_types <- unlist(habitat_types)

  habitat_types <- structure(
    data.frame(
      code = sub("SpecificType", "", sub("MKV.habitat", "", habitat_types))
    ),
    row.names = habitat_types
  )

  for (i in row.names(habitat_types)) {

    status <- httr::GET(
      "https://tun.fi", path = i, query = list(format = "json")
    )
    status <- httr::content(status)

    if (hasName(status[["label"]], "@language")) {

      habitat_types[i, paste0("name_", status[["label"]][["@language"]])] <-
        status[["label"]][["@value"]]

    } else {

      for (j in status[["label"]]) {

        habitat_types[i, paste0("name_", j[["@language"]])] <- j[["@value"]]

      }

    }

  }

  habitat_types <- t(
    apply(habitat_types, 1L, function(x) ifelse(is.na(x), x["name_fi"], x))
  )

  habitat_types <- as.data.frame(habitat_types)

  class(habitat_types[["code"]]) <- "translation"

  habitat_types

}

primary_habitat <- lapply(
  c("MKV.habitatEnum", "MKV.habitatSpecificTypeEnum"),
  get_habitat_types
)

names(primary_habitat) <- c("habitat_types", "specific_habitat_types")

primary_secondary_habitat <- primary_habitat
