source("data-raw/utils.R")
metadata_ranges <-
  finbif:::api_get("metadata/ranges", list(lang = "multi"), FALSE)[["content"]]
atlas_code <- lapply(
  metadata_ranges[["MY.atlasCodeEnum"]],  as.data.frame,
  stringsAsFactors = FALSE
)
atlas_code <- reduce_merge(atlas_code)
row.names(atlas_code) <- atlas_code[["id"]]
atlas_code[["id"]] <- NULL

names(atlas_code) <- sub("value\\.", "name_", names(atlas_code))

atlas_class <- lapply(
  metadata_ranges[["MY.atlasClassEnum"]], as.data.frame,
  stringsAsFactors = FALSE
)
atlas_class <- reduce_merge(atlas_class)
row.names(atlas_class) <- atlas_class[["id"]]
atlas_class[["id"]] <- NULL
names(atlas_class) <- sub("value\\.", "name_", names(atlas_class))

for (i in names(atlas_code)) {

  class(atlas_code[[i]]) <- "translation"
  class(atlas_class[[i]]) <- "translation"

}
