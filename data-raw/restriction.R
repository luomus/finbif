source("data-raw/utils.R")
enums <- finbif:::api_get("warehouse/enumeration-labels", list(), FALSE)
enums <- enums[["content"]][["results"]]
enums <- lapply(enums, as.data.frame, stringsAsFactors = FALSE)
enums <- reduce_merge(enums)

metadata_ranges <-
  finbif:::api_get("metadata/ranges", list(), FALSE)[["content"]]

restriction_level <- lapply(
  metadata_ranges[["MX.secureLevels"]], as.data.frame, stringsAsFactors = FALSE
)

restriction_level <- reduce_merge(restriction_level)

restriction_level <- merge(
  restriction_level, enums, by.x = "id", by.y = "property",
  stringsAsFactors = FALSE
)

restriction_level <- restriction_level[c("enumeration", "value")]

restriction_level[["enumeration"]] <-
  tolower(restriction_level[["enumeration"]])
