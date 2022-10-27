source("data-raw/utils.R")
metadata_ranges <-
  finbif:::api_get("metadata/ranges", list(lang = "multi"), FALSE)[["content"]]
atlas_code <- lapply(
  metadata_ranges[["MY.atlasCodeEnum"]],  as.data.frame, stringsAsFactors = FALSE
)
atlas_code <- reduce_merge(atlas_code)
row.names(atlas_code) <- atlas_code[["id"]]
atlas_code[["id"]] <- NULL

cols <- c("name_en", "name_fi", "name_sv")
names(atlas_code) <- cols

atlas_class <- lapply(
  metadata_ranges[["MY.atlasClassEnum"]], as.data.frame,
  stringsAsFactors = FALSE
)
atlas_class <- reduce_merge(atlas_class)
row.names(atlas_class) <- atlas_class[["id"]]
atlas_class[["id"]] <- NULL
names(atlas_class) <- cols

for (i in cols) {

  class(atlas_code[[i]]) <- "translation"
  class(atlas_class[[i]]) <- "translation"

}
