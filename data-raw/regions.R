source("data-raw/utils.R")
n <- 1000L
region <- finbif:::api_get(
  "areas", list(type = "province", lang = "multi", page = 1L, pageSize = n),
  FALSE
)
stopifnot(n > region[["content"]][["total"]])
region <- region[["content"]][["results"]]
region <- lapply(region, as.data.frame, stringsAsFactors = FALSE)
region <- reduce_merge(region)

row.names(region) <- region[["id"]]

region <- region[order(region[["name.en"]]), c("name.en", "name.fi", "name.sv")]

names(region) <- c("name_en", "name_fi", "name_sv")

class(region[["name_en"]]) <- "translation"
class(region[["name_fi"]]) <- "translation"
class(region[["name_sv"]]) <- "translation"
