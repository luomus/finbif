metadata_ranges <-
  finbif:::finbif_api_get("metadata/ranges", list(), FALSE)[["content"]]

habitat_types <-
  metadata_ranges[c("MKV.habitatEnum", "MKV.habitatSpecificTypeEnum")]
habitat_types <- lapply(habitat_types, unlist)
habitat_types <- lapply(habitat_types, matrix, ncol = 2L, byrow = TRUE)

habitat_types <- lapply(
  habitat_types,
  function(x) {
    df <- data.frame(
      stringr::str_split_fixed(x[, 2L], " ", 3L)[, c(3L, 1L)],
      row.names = x[, 1L],
      stringsAsFactors = FALSE
    )
    df <- stats::setNames(df, c("habitat_type", "code"))
    df[["habitat_type"]] <- gsub("ä", "a", df[["habitat_type"]])
    df[["habitat_type"]] <- finbif:::to_sentence_case(df[["habitat_type"]])
    df[["code"]] <- toupper(df[["code"]])
    df
  }
)

names(habitat_types) <- c("habitat_types", "specific_habitat_types")
