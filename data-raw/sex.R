metadata_ranges <-
  finbif:::api_get("metadata/ranges", list(), FALSE)[["content"]]

sex <- vapply(metadata_ranges[["MY.sexes"]], getElement, "", "value")

sex <-
  data.frame(do.call(rbind, strsplit(sex, " - ")), stringsAsFactors = FALSE)

names(sex) <- c("code", "category")

sex[["category"]] <- tolower(sex[["category"]])
row.names(sex) <- sub(" ", "_", sex[["category"]])

class(sex[["code"]]) <- "translation"
class(sex[["category"]]) <- "translation"
