source("data-raw/utils.R")
metadata_ranges <-
  finbif:::api_get("metadata/ranges", list(), FALSE)[["content"]]
complete_list_type <- lapply(
  metadata_ranges[["MY.completeListTypeEnum"]],  as.data.frame,
  stringsAsFactors = FALSE
)
complete_list_type <- reduce_merge(complete_list_type)
row.names(complete_list_type) <- complete_list_type[["id"]]
complete_list_type[["id"]] <- NULL
names(complete_list_type) <- "type"

complete_list_type[["MY.completeListTypeCompleteWithBreedingStatus", "type"]] <-
  "all_species_and_all_breeding"

complete_list_type[["MY.completeListTypeComplete", "type"]] <-
  "all_species_and_partial_breeding"

complete_list_type[["MY.completeListTypeIncomplete", "type"]] <-
  "incomplete"

complete_list_type <- rbind(
  complete_list_type,
  data.frame(
    type = "all_species",
    row.names =
      "MY.completeListTypeCompleteWithBreedingStatus,MY.completeListTypeComplete"
  )
)

class(complete_list_type[["type"]]) <- "translation"
