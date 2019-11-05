source("data-raw/utils.R")
n <- 1000L
source <-
  finbif:::finbif_api_get("sources", list(lang = "multi", pageSize = n), FALSE)
stopifnot(n > source[["content"]][["total"]])
source <- source[["content"]][["results"]]
source <- lapply(source, as.data.frame, stringsAsFactors = FALSE)
source <- reduce_merge(source)
row.names(source) <- source[["id"]]
names(source) <- sub("\\.", "_", names(source))

source[which(source[["description_en"]] == ""), "description_en"] <-
  NA_character_
source[which(source[["description_fi"]] == ""), "description_fi"] <-
  NA_character_
source[which(source[["description_sv"]] == ""), "description_sv"] <-
  NA_character_

class(source[["id"]]) <- "translation"
class(source[["name_en"]]) <- "translation"
class(source[["name_fi"]]) <- "translation"
class(source[["name_sv"]]) <- "translation"
