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
#' @param locale Character. Language of data returned. One of "en", "fi", or
#'   "sv".
#' @param nmin Integer. Filter collections by number of records. Only return
#'   information on collections with greater than value specified. If `NA` then
#'   return information on all collections.
#' @param cache Logical. Use cached data.
#' @return A data.frame.
#' @examples \dontrun{
#'
#' # Get collection metadata
#' collections <- finbif_collections()
#' }
#' @export

finbif_collections <- function(
  filter,
  select,
  subcollections = TRUE,
  supercollections = FALSE,
  locale = getOption("finbif_locale"),
  nmin = 0,
  cache = getOption("finbif_use_cache")
) {

  locale <- switch(locale, sv = locale, fi = locale, "en")

  swagger <- get_swagger(cache)

  swagger <- httr::content(swagger)

  swagger <- swagger[["definitions"]]

  col_md_nms <- swagger[["Collection"]]

  col_md_nms <- col_md_nms[["properties"]]

  col_md_nms <- names(col_md_nms)

  qry <- list(lang = locale)

  col_md <- list(
    qry = qry, path = "collections", nms = col_md_nms, id = "id", cache = cache
  )

  col_md <- get_collections(col_md)

  col_count_nms <- swagger[["DwQuery_AggregateRow"]]

  col_count_nms <- col_count_nms[["properties"]]

  col_count_nms <- names(col_count_nms)

  qry <- list(
    aggregateBy = "document.collectionId",
    onlyCount = FALSE,
    pessimisticDateRangeHandling = TRUE
  )

  finbif_warehouse_query <- getOption("finbif_warehouse_query")

  path <- paste0(finbif_warehouse_query, "unit/aggregate")

  col_counts <- list(
    qry = qry,
    path = path,
    nms = col_count_nms,
    id = "aggregateBy",
    cache = cache
  )

  col_counts <- get_collections(col_counts)

  collections <- merge(
    col_md, col_counts, by.x = "id", by.y = "aggregate_by", all.x = TRUE
  )

  descriptions <- collections[["description"]]

  collections[["data_description"]] <- descriptions

  data_quality_description <- collections[["data_quality_description"]]

  na_data_quality_description <- is.na(data_quality_description)

  descriptions_with_quality <- paste(
    descriptions, data_quality_description, sep = "\nData quality: "
  )

  collections[["description"]] <- ifelse(
    na_data_quality_description, descriptions, descriptions_with_quality
  )

  collection_ids <- collections[["id"]]

  row.names(collections) <- collection_ids

  # Sometimes collections don't have a "has_children" field
  has_children <- collections[["has_children"]]

  has_children_not_na <- !is.na(has_children)

  has_children_and_not_na <- has_children & has_children_not_na

  parent_collections <- collection_ids[has_children_and_not_na]

  collections_part_of <- collections[["is_part_of"]]

  for (collection in parent_collections) {

    collection_count <- collections[collection, "count"]

    collection_count_na <- is.na(collection_count)

    if (collection_count_na) {

      collection_part_of <- collections_part_of == collection

      collection_part_of_counts <- collections[collection_part_of, "count"]

      collection_count <- sum(collection_part_of_counts, na.rm = TRUE)

      collections[collection, "count"] <- collection_count

    }

  }

  has_nmin <- !is.na(nmin)

  if (has_nmin) {

    collections_count <- collections[["count"]]

    has_count <- !is.na(collections_count)

    collection_count_enough <- collections_count > nmin

    include_rows <- has_count & collection_count_enough

    collections <- collections[include_rows, ]

  }

  if (!subcollections) {

    include_rows <- is.na(collections_part_of)

    collections <- collections[include_rows, ]

  }

  if (!supercollections) {

    collections <- collections[!has_children, ]

  }

  n_collections <- nrow(collections)

  rows <- rep_len(TRUE, n_collections)

  has_filter <- !missing(filter)

  parent_frame <- parent.frame()

  if (has_filter) {

    call <- substitute(filter)

    rows <- eval(call, collections, parent_frame)

    rows_are_logical <- !is.logical(rows)

    if (rows_are_logical) {

      deferrable_error("Collections filter must be a logical vector")

    }

    rows_not_na <- !is.na(rows)

    rows <- rows & rows_not_na

  }

  has_select <- !missing(select)

  cols <- c(
    "collection_name",
    "abbreviation",
    "description",
    "online_url",
    "has_children",
    "is_part_of",
    "data_quality",
    "methods",
    "collection_type",
    "taxonomic_coverage",
    "geographic_coverage",
    "temporal_coverage",
    "secure_level",
    "count"
  )

  if (has_select)  {

    cols_seq <- seq_along(collections)

    cols_seq <- as.list(cols_seq)

    col_names <- names(collections)

    names(cols_seq) <- col_names

    call <- substitute(select)

    cols <- eval(call, cols_seq, parent_frame)

    na_cols <- is.na(cols)

    all_cols_na <- all(na_cols)

    cols_null <- is.null(cols)

    cond <- all_cols_na || cols_null

    if (cond) {

      cols <- TRUE

    }

  }

  collections <- collections[rows, cols, drop = FALSE]

  class <- c("finbif_collections", "finbif_metadata_df", "data.frame")

  structure(collections, class = class)

}

get_collections <- function(obj) {
  nms <- obj[["nms"]]
  qry <- c(obj[["qry"]], list(page = 0L, pageSize = 1000L))
  collections <- list()
  total <- 1L

  while (total > qry[["page"]] * qry[["pageSize"]]) {

    qry[["page"]] <- qry[["page"]] + 1L

    collections[[qry[["page"]]]] <- api_get(
      list(
        path = obj[["path"]],
        query = qry,
        cache = obj[["cache"]]
      )
    )

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

  collections[[obj[["id"]]]] <- gsub(
    "^http:\\/\\/tun\\.fi\\/", "", collections[[obj[["id"]]]]
  )

  names(collections) <- sub("\\.", "_", names(collections))
  names(collections) <- gsub(
    "([a-z])([A-Z])", "\\1_\\L\\2", names(collections), perl = TRUE
  )

  collections

}

#' @noRd
#' @importFrom digest digest
#' @importFrom httr RETRY

get_swagger <- function(cache) {

  url <- paste0(getOption("finbif_api_url"), "/explorer/swagger.json")

  if (cache) {

    hash <- digest::digest(url)

    fcp <- getOption("finbif_cache_path")

    if (is.null(fcp)) {

      ans <- get_cache(hash)

      if (!is.null(ans)) {

        return(ans)

      }

      on.exit(

        if (!is.null(ans)) {

          set_cache(list(data = ans, hash = hash))

        }

      )

    } else {

      cache_file <- file.path(fcp, paste0("finbif_cache_file_", hash))

      if (file.exists(cache_file)) {

        return(readRDS(cache_file))

      }

      on.exit(

        if (!is.null(ans)) {

          saveRDS(ans, cache_file)

        }

      )

    }

  }

  stopifnot(
    "Request not cached and option:finbif_allow_query = FALSE" =
      getOption("finbif_allow_query")
  )

  Sys.sleep(1 / getOption("finbif_rate_limit"))

  ans <- httr::RETRY(
    "GET",
    url,
    times = getOption("finbif_retry_times"),
    pause_base = getOption("finbif_retry_pause_base"),
    pause_cap = getOption("finbif_retry_pause_cap"),
    pause_min = getOption("finbif_retry_pause_min"),
    terminate_on = 404L
  )

  ans

}
