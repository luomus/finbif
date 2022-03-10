source("data-raw/utils.R")
metadata_ranges <-
  finbif:::api_get("metadata/ranges", list(), FALSE)[["content"]]
location_tag <- lapply(
  metadata_ranges[["MNP.tagEnum"]],  as.data.frame, stringsAsFactors = FALSE
)
location_tag <- reduce_merge(location_tag)
row.names(location_tag) <- location_tag[["id"]]
location_tag[["id"]] <- NULL
names(location_tag) <- "tag"

location_tag[["tag"]] <- tolower(location_tag[["tag"]])
location_tag[["tag"]] <- gsub(" ", "_", location_tag[["tag"]])
location_tag[["tag"]] <- gsub("accessibility", "access", location_tag[["tag"]])

class(location_tag[["tag"]]) <- "translation"
