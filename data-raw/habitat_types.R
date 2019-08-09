metadata_ranges <-
  finbif:::finbif_api_get("metadata/ranges", list(), FALSE)[["content"]]

primary_habitat <-
  metadata_ranges[c("MKV.habitatEnum", "MKV.habitatSpecificTypeEnum")]
primary_habitat <- lapply(primary_habitat, unlist)
primary_habitat <- lapply(primary_habitat, matrix, ncol = 2L, byrow = TRUE)

primary_habitat <- lapply(
  primary_habitat,
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
    class(df[["code"]]) <- "translation"
    df
  }
)

names(primary_habitat) <- c("habitat_types", "specific_habitat_types")
primary_secondary_habitat <- primary_habitat
