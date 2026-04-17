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
#' @param cache Logical or Integer. If `TRUE` or a number greater than zero,
#'   then data-caching will be used. If not logical then cache will be
#'   invalidated after the number of hours indicated by the argument.
#' @return A data.frame.
#' @examples \dontrun{
#'
#' # Get collection metadata
#' collections <- finbif_collections()
#'
#' }
#' @export
finbif_collections <- function(
  filter,
  select,
  subcollections = TRUE,
  supercollections = FALSE,
  locale = getOption("finbif_locale"),
  nmin = 0,
  cache = getOption("finbif_use_cache_metadata")
) {
  locale <- switch(locale, sv = locale, fi = locale, "en")
  swagger <- list(path = "swagger", cache = cache)
  swagger <- api_get(swagger)
  swagger <- swagger[[c("content", "components", "schemas")]]

  col_md_nms <- swagger[[c("SensitiveCollection", "properties")]]
  col_md_nms <- names(col_md_nms)
  col_md_nms <- grep("@", col_md_nms, value = TRUE, invert = TRUE)

  col_count_nms <- swagger[[c("WarehouseDwQuery_AggregateRow", "properties")]]
  col_count_nms <- names(col_count_nms)
  col_count_nms <- unique(col_count_nms)
  col_count_nms <- grep("@", col_count_nms, value = TRUE, invert = TRUE)

  col_nms <- c(col_md_nms, col_count_nms)

  col_nms_snk <- sub("\\.", "_", col_nms)
  col_nms_snk <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", col_nms_snk, perl = TRUE)
  col_nms_snk <- tolower(col_nms_snk)

  col_type <- c(
    rep("md", length(col_md_nms)), rep("count", length(col_count_nms))
  )

  col_df <- data.frame(nms = col_nms, type = col_type)

  rownames(col_df) <- col_nms_snk

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

  if (!missing(select)) {
    cols_seq <- seq_along(col_nms)
    cols_seq <- as.list(cols_seq)
    names(cols_seq) <- col_nms_snk
    call <- substitute(select)
    cols <- eval(call, cols_seq, parent.frame())

    if (is.null(cols) || all_na(cols)) cols <- col_nms_snk
  }

  cols <- rownames(col_df[cols, ])

  col_df <- col_df[unique(c("id", "aggregate_by", cols)), ]

  col_md <- list(
    qry = NULL,
    lang = locale,
    path = "collections",
    nms = col_df[col_df[["type"]] == "md", "nms"],
    id = "id",
    cache = cache
  )

  collections <- get_collections(col_md)

  if ("count" %in% col_df[["type"]]) {
    qry <- list(
      aggregateBy = "document.collectionId",
      onlyCount = FALSE,
      pessimisticDateRangeHandling = TRUE
    )

    col_counts <- list(
      qry = qry,
      lang = locale,
      path = paste0(getOption("finbif_warehouse_query"), "unit/aggregate"),
      nms = col_df[col_df[["type"]] == "count", "nms"],
      id = "aggregateBy",
      cache = cache
    )
    col_counts <- get_collections(col_counts)

    collections <- merge(
      collections, col_counts, by.x = "id", by.y = "aggregate_by", all.x = TRUE
    )
    cols <- setdiff(cols, "aggregate_by")
  }

  if ("description" %in% cols && "data_quality_description" %in% cols) {
    descriptions <- collections[["description"]]
    data_quality_description <- collections[["data_quality_description"]]
    na_data_quality_description <- is.na(data_quality_description)
    descriptions_with_quality <- paste(
      descriptions, data_quality_description, sep = "\nData quality: "
    )

    collections[["description"]] <- ifelse(
      na_data_quality_description, descriptions, descriptions_with_quality
    )
  }

  row.names(collections) <- collections[["id"]]

  has_children <- collections[["has_children"]]
  has_children <- has_children & !is.na(has_children)

  for (collection in collections[has_children, "id"]) {
    if (is.na(collections[collection, "count"])) {
      collection_part_of <- collections[["is_part_of"]] == collection
      collections[collection, "count"] <- sum(
        collections[collection_part_of, "count"], na.rm = TRUE
      )
    }
  }

  ind <- TRUE

  if (!is.na(nmin)) {
    ind <- ind & !is.na(collections[["count"]]) & collections[["count"]] >= nmin
  }

  if (!subcollections) {
    ind <- ind & is.na(collections[["is_part_of"]])
  }

  if (!supercollections) {
    ind <- ind & !has_children
  }

  if (!getOption("finbif_use_all_collections")) {
    ind <- ind & !collections[["id"]] %in% without_collections()
    ind <- ind & !collections[["is_part_of"]] %in% without_collections()
  }

  collections <- collections[ind, , drop = FALSE]

  n_collections <- nrow(collections)
  rows <- rep_len(TRUE, n_collections)

  if (!missing(filter)) {
    call <- substitute(filter)
    rows <- eval(call, collections, parent.frame())

    if (!is.logical(rows)) {
      deferrable_error("Collections filter must be a logical vector")
    }

    rows <- rows & !is.na(rows)
  }

  structure(
    collections[rows, cols, drop = FALSE],
    class = c("finbif_collections", "finbif_metadata_df", "data.frame")
  )
}

#' @noRd
get_collections <- function(col_obj) {
  page <- 0L
  page_size <- 1000L
  page_args <- list(page = page, pageSize = page_size)
  qry <- c(col_obj[["qry"]], page_args)
  cache <- col_obj[["cache"]]
  lang <- col_obj[["lang"]]
  collections_list <- list()

  cond <- TRUE

  while (cond) {
    page <- page + 1L
    qry[["page"]] <- page
    query_obj <- list(
      path = col_obj[["path"]], query = qry, cache = cache, lang = lang
    )
    resp <- api_get(query_obj)
    collections_list[[page]] <- resp
    cond <- resp[[c("content", "total")]] > page * page_size
  }

  for (i in c("content", "results")) {
    collections_list <- lapply(collections_list, "[[", i)
  }

  collections_list <- do.call(c, collections_list)

  for (i in seq_along(collections_list)) {
    collections_i <- collections_list[[i]]

    for (nm in col_obj[["nms"]]) {
      if (is.null(collections_i[[nm]])) {
        collections_i[[nm]] <- NA
      }
    }

    collections_list[[i]] <- collections_i
  }

  collections <- list()

  for (nm in col_obj[["nms"]]) {
    collections[[nm]] <- lapply(collections_list, "[[", nm)
  }

  lth_of_els <- lapply(collections, lapply, length)
  lth_of_els <- lapply(lth_of_els, unlist)
  lth_of_els <- vapply(lth_of_els, max, 0L)
  nms <- split(col_obj[["nms"]], lth_of_els > 1L)
  nms_mt_one_el <- nms[["TRUE"]]
  list_cols <- collections[nms_mt_one_el, drop = FALSE]
  nms_one_el <- nms[["FALSE"]]
  one_el_cols <- collections[nms_one_el, drop = FALSE]
  collections <- lapply(one_el_cols, unlist)
  collections <- as.data.frame(collections, col.names = nms_one_el)
  collections[nms_mt_one_el] <- list_cols
  id <- col_obj[["id"]]
  collections[[id]] <- sub("^http:\\/\\/tun\\.fi\\/", "", collections[[id]])
  col_names <- names(collections)
  col_names <- sub("\\.", "_", col_names)
  col_names <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", col_names, perl = TRUE)
  col_names <- tolower(col_names)
  structure(collections, names = col_names)
}
