metadata_ranges <-
  finbif:::finbif_api_get("v0/metadata/ranges", list(), FALSE)$content

habitat_types <- matrix(
  unlist(metadata_ranges$MKV.habitatEnum), ncol = 2, byrow = TRUE
)

habitat_types <- data.frame(
  stringr::str_split_fixed(habitat_types[, 2], " ", 3)[, c(3, 1)],
  row.names = habitat_types[, 1],
  stringsAsFactors = FALSE
)

colnames(habitat_types) <- c("habitat_type", "code")

habitat_types$habitat_type <- gsub("ä", "a", habitat_types$habitat_type)

habitat_types$habitat_type <- finbif:::to_sentence_case(
  habitat_types$habitat_type
)

habitat_types$code <- toupper(habitat_types$code)

# Also need specific type
