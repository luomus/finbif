#' FinBIF collections
#'
#' Get information on collections in the FinBIF database.
#'
#' @param subcollections Logical. Return subcollection metadata of higher level
#'   collections.
#' @param supercollections Logical. Return lowest level collection metadata.
#' @param nmin Integer. Filter collections by number of records. Only return
#'   information on collections with greater than value specified. If `NA` then
#'   return information on all collections.
#' @return A data.frame.
#' @examples \dontrun{
#'
#' # Get collection metadata
#' collecitons <- finbif_collections()
#' }
#' @importFrom utils hasName
#' @export
finbif_collections <- function(
  subcollections = FALSE, supercollections = TRUE, nmin = 0
) {

  collections <- merge(
    get_collections(list(lang = "multi"), "collections"),
    get_collections(
      list(
        aggregateBy = "document.collectionId", onlyCount = FALSE,
        pessimisticDateRangeHandling = TRUE
      ),
      "warehouse/query/aggregate"
    ),
    by.x = "id",
    by.y = "document_collection_id",
    all.x = TRUE
  )

  row.names(collections) <- collections[["id"]]

  collections <- collections[, -1L]

  parent_collections <- row.names(collections)[collections[["has_children"]]]

  for (collection in parent_collections)
    collections[collection, "count"] <- sum(
      collections[collections[["is_part_of"]] == collection, "count"],
      na.rm = TRUE
    )

  if (!is.na(nmin))
    collections <- collections[
      !is.na(collections[["count"]]) & collections[["count"]] > nmin,
    ]

  if (!subcollections)
    collections <- collections[is.na(collections[["is_part_of"]]), ]

  if (!supercollections)
    collections <- collections[!collections[["has_children"]], ]

  collections

}

get_collections <- function(qry, path) {
  qry <- c(qry, list(page = 0L, pageSize = 1000L))
  collections <- list()
  total <- 1L

  while (total > qry[["page"]] * qry[["pageSize"]]) {
    qry[["page"]] <- qry[["page"]] + 1L
    collections[[qry[["page"]]]] <- finbif_api_get(path, qry, TRUE)
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

  collections[[1L]] <- gsub("^http:\\/\\/tun\\.fi\\/", "", collections[[1L]])

  names(collections) <- sub("\\.", "_", names(collections))
  names(collections) <-
    gsub("([a-z])([A-Z])", "\\1_\\L\\2", names(collections), perl = TRUE)
  collections
}
