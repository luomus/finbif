n <- 1000L
collections <- finbif:::finbif_api_get(
  "collections", list(lang = "multi", page = 1L, pageSize = n), FALSE
)
stopifnot(n > collections[["content"]][["total"]])

collections <- collections[["content"]][["results"]]
collections <- lapply(
  collections,
  function(x) {
    nm <- "downloadRequestHandler"
    if (hasName(x, nm)) names(x[[nm]]) <- paste0(nm, seq_along(x[[nm]]))
    as.data.frame(x, stringsAsFactors = FALSE)
  }
)
collections <- finbif:::reduce_merge(collections)
