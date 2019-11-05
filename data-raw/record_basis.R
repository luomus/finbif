source("data-raw/utils.R")
metadata_ranges <-
  finbif:::finbif_api_get("metadata/ranges", list(), FALSE)[["content"]]
record_basis <- lapply(
  metadata_ranges[["MY.recordBases"]],  as.data.frame, stringsAsFactors = FALSE
)
record_basis <- reduce_merge(record_basis)
row.names(record_basis) <- record_basis[["id"]]
record_basis[["id"]] <- NULL
names(record_basis) <- "description"

record_basis[["description"]] <-
  sub("catched", "caught", record_basis[["description"]])
record_basis[["description"]] <-
  sub(" etc", " etc.", record_basis[["description"]])
record_basis[["name"]] <-
  sub("( \\(.*\\)){0,1}( specimen){0,1}$", "", record_basis[["description"]])
record_basis[["name"]] <-
  tolower(gsub("( from){0,1} ", "_", record_basis[["name"]]))

class(record_basis[["name"]]) <- "translation"

superrecord_basis <- data.frame(
  options = c("human_observation", "machine_observation", "specimen"),
  row.names = c(
    "human_observation_unspecified", "machine_observation_unspecified",
    "preserved_specimen"
  ),
  stringsAsFactors = FALSE
)

class(superrecord_basis[["options"]]) <- "translation"
