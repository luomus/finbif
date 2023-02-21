source("data-raw/utils.R")

metadata_ranges <-
  finbif:::api_get(
    list(path = "metadata/ranges", query = list(), cache = FALSE)
  )[["content"]]

labels <-
  finbif:::api_get(
    list(path = "warehouse/enumeration-labels", query = list(), cache = FALSE)
  )[["content"]]

labels <- reduce_merge(labels[["results"]])

sex <- lapply(
  metadata_ranges[["MY.sexes"]], as.data.frame, stringsAsFactors = FALSE
)

sex <- reduce_merge(sex)

sex <- merge(
  sex[1L], labels, by.x = "id", by.y = "property"
)

sex[["id"]] <- NULL

row.names(sex) <- sex[["enumeration"]]
sex[["enumeration"]] <- NULL

sex_en <- do.call(rbind, strsplit(sex[["label.en"]], " - "))

sex[["label.en"]] <- tolower(sex_en[, 2L])
sex[["code"]] <- sex_en[, 1L]

cols <- sub("label\\.", "name_", names(sex))

names(sex) <- cols

sex <- sex[sort(cols)]

for (i in cols) {

  class(sex[[i]]) <- "translation"

}
