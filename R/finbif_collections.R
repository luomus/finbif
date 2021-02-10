#' FinBIF collections
#'
#' Get information on collections in the FinBIF database.
#'
#' @aliases fb_collections
#'
#' @param filter Logical. Expression indicating elements or rows to keep:
#'   missing values are taken as false.
#' @param select Expression. Indicates columns to select from the data frame.
#' @param subcollections Logical. Return subcollection metadata of higher level
#'   collections.
#' @param supercollections Logical. Return lowest level collection metadata.
#' @param lang Character. Language of data returned. One of "en", "fi", or "sv".
#' @param nmin Integer. Filter collections by number of records. Only return
#'   information on collections with greater than value specified. If `NA` then
#'   return information on all collections.
#' @param cache Logical. Use cached data.
#' @return A data.frame.
#' @examples \dontrun{
#'
#' # Get collection metadata
#' collecitons <- finbif_collections()
#' }
#' @importFrom utils hasName
#' @export

finbif_collections <- function(
  filter, select, subcollections = TRUE, supercollections = FALSE,
  lang = c("en", "fi", "sv"), nmin = 0, cache = getOption("finbif_use_cache")
) {

  lang <- match.arg(lang)

  swagger <- httr::GET("https://api.laji.fi/explorer/swagger.json")
  swagger <-
    jsonlite::fromJSON(httr::content(swagger, "text"), simplifyVector = FALSE)
  col_md_nms <- names(swagger[["definitions"]][["Collection"]][["properties"]])
  col_md <- get_collections(
    list(lang = lang), "collections", col_md_nms, "id", cache
  )

  col_count_nms <- names(
    swagger[["definitions"]][["DwQuery_AggregateRow"]][["properties"]]
  )
  col_count <- get_collections(
    list(
      aggregateBy = "document.collectionId", onlyCount = FALSE,
      pessimisticDateRangeHandling = TRUE
    ),
    "warehouse/query/unit/aggregate", col_count_nms, "aggregateBy", cache
  )

  collections <- merge(
    col_md, col_count, by.x = "id", by.y = "aggregate_by", all.x = TRUE
  )

  row.names(collections) <- collections[["id"]]
  # Sometimes collections dont have a "has_children" field
  ind <- collections[["has_children"]]
  ind <- ind & !is.na(ind)
  parent_collections <- row.names(collections)[ind]

  for (collection in parent_collections) {
    if (is.na(collections[collection, "count"]))
      collections[collection, "count"] <- sum(
        collections[collections[["is_part_of"]] == collection, "count"],
        na.rm = TRUE
      )
  }

  if (!is.na(nmin)) {
    collections <- collections[
      !is.na(collections[["count"]]) & collections[["count"]] > nmin,
    ]
  }

  if (!subcollections) {
    collections <- collections[is.na(collections[["is_part_of"]]), ]
  }

  if (!supercollections) {
    collections <- collections[!collections[["has_children"]], ]
  }

  if (missing(filter)) {
    rows <- rep_len(TRUE, nrow(collections))
  } else {
    call <- substitute(filter)
    rows <- eval(call, collections, parent.frame())
    if (!is.logical(rows)) {
       deferrable_error("'Collections filter must be logical")
    }
    rows <- rows & !is.na(rows)
  }

  if (missing(select)) {
    cols <- c(
      "collection_name", "abbreviation", "description", "online_url",
      "has_children", "is_part_of", "data_quality", "methods",
      "collection_type", "taxonomic_coverage", "geographic_coverage",
      "temporal_coverage", "secure_level", "count"
    )
  } else {
    col_ind <- as.list(seq_along(collections))
    names(col_ind) <- names(collections)
    cols <- eval(substitute(select), col_ind, parent.frame())
    if (is.na(cols) || is.null(cols)) cols <- TRUE
  }

  structure(
    collections[rows, cols, drop = FALSE],
    class = c("finbif_collections", "finbif_metadata_df", "data.frame")
  )

}

get_collections <- function(qry, path, nms, id, cache) {
  qry <- c(qry, list(page = 0L, pageSize = 1000L))
  collections <- list()
  total <- 1L

  while (total > qry[["page"]] * qry[["pageSize"]]) {
    qry[["page"]] <- qry[["page"]] + 1L
    collections[[qry[["page"]]]] <- api_get(path, qry, cache)
    total <- collections[[qry[["page"]]]][["content"]][["total"]]
  }

  for (i in c("content", "results")) {
    collections <- lapply(collections, getElement, i)
  }

  collections <- do.call(c, collections)

  collections <- lapply(
    seq_along(nms),
    function(i) {
      lapply(
        collections,
        function(x) {
          ans <- getElement(x, nms[i])
          if (is.null(ans)) NA else ans
        }
      )
    }
  )

  names(collections) <- nms

  lth_of_els <- lapply(collections, function(x) max(unlist(lapply(x, length))))
  nms <- split(nms, lth_of_els > 1L)

  list_cols <- collections[nms[["TRUE"]]]

  collections <- lapply(collections[nms[["FALSE"]]], unlist)

  collections <- as.data.frame(
    collections, col.names = nms[["FALSE"]], stringsAsFactors = FALSE
  )

  collections[nms[["TRUE"]]] <- list_cols

  collections[[id]] <- gsub(
    "^http:\\/\\/tun\\.fi\\/", "", collections[[id]]
  )

  names(collections) <- sub("\\.", "_", names(collections))
  names(collections) <- gsub(
    "([a-z])([A-Z])", "\\1_\\L\\2", names(collections), perl = TRUE
  )

  collections

}
