#' FinBIF collections
#'
#' Display information on the collections in the FinBIF database.
#'
#' @param subcollections Logical. Display subcollections as well the main
#'   collections.
#' @return A data.frame.
#' @examples \dontrun{
#'
#' # Display the informal taxonomic groups used by FinBIF
#' finbif_collections()
#' }
#' @importFrom utils hasName
#' @export
finbif_collections <- function(subcollections = FALSE) {

  qry <- list(lang = "multi", page = 0L, pageSize = 1000L)
  collections <- list()
  total <- 1L

  while (total > qry[["page"]] * qry[["pageSize"]]) {
    qry[["page"]] <- qry[["page"]] + 1L
    collections[[qry[["page"]]]] <- finbif_api_get("collections", qry, TRUE)
    total <- collections[[qry[["page"]]]][["content"]][["total"]]
  }

  for (i in c("content", "results"))
    collections <- lapply(collections, getElement, i)

  collections <- do.call(c, collections)

  collections <- lapply(
    collections,
    function(x) {
      nm <- "downloadRequestHandler"
      if (utils::hasName(x, nm)) names(x[[nm]]) <- nm
      as.data.frame(x, stringsAsFactors = FALSE)
    }
  )

  collections <- reduce_merge(collections)

  names(collections) <- sub("\\.", "_", names(collections))
  names(collections) <-
    gsub("([a-z])([A-Z])", "\\1_\\L\\2", names(collections), perl = TRUE)

  if (subcollections) {
    collections
  } else {
    collections[
      is.na(collections[["is_part_of"]]),
      setdiff(names(collections), "is_part_of")
    ]
  }

}
