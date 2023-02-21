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

abundance_unit <- lapply(
  metadata_ranges[["MY.abundanceUnitEnum"]], as.data.frame,
  stringsAsFactors = FALSE
)

abundance_unit <- reduce_merge(abundance_unit)

abundance_unit <- merge(
  abundance_unit[1L], labels, by.x = "id", by.y = "property"
)

abundance_unit[["id"]] <- NULL

row.names(abundance_unit) <- abundance_unit[["enumeration"]]
abundance_unit[["enumeration"]] <- NULL

cols <- sub("label\\.", "name_", names(abundance_unit))

names(abundance_unit) <- cols

abundance_unit <- abundance_unit[sort(cols)]

for (i in cols) {

  class(abundance_unit[[i]]) <- "translation"

}

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

record_quality <- lapply(
  metadata_ranges[["MZ.recordQualityEnum"]], as.data.frame,
  stringsAsFactors = FALSE
)

record_quality <- reduce_merge(record_quality)

record_quality <- merge(
  record_quality[1L], labels, by.x = "id", by.y = "property"
)

record_quality[["id"]] <- NULL

row.names(record_quality) <- record_quality[["enumeration"]]

cols <- sub("label\\.", "name_", names(record_quality))

names(record_quality) <- cols

record_quality <- record_quality[sort(cols)]

for (i in cols) {

  class(record_quality[[i]]) <- "translation"

}

collection_quality <- lapply(
  metadata_ranges[["MY.collectionQualityEnum"]], as.data.frame,
  stringsAsFactors = FALSE
)

collection_quality <- reduce_merge(collection_quality)

collection_quality <- merge(
  collection_quality[1L], labels, by.x = "id", by.y = "property"
)

collection_quality[["id"]] <- NULL

row.names(collection_quality) <- collection_quality[["enumeration"]]

cols <- sub("label\\.", "name_", names(collection_quality))

names(collection_quality) <- cols

collection_quality <- collection_quality[sort(cols)]

for (i in cols) {

  class(collection_quality[[i]]) <- "translation"

}
