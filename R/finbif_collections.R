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
#' @importFrom httr content
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

    cond <- is.null(cols) || all_na(cols)

    if (cond) {

      cols <- TRUE

    }

  }

  collections <- collections[rows, cols, drop = FALSE]

  class <- c("finbif_collections", "finbif_metadata_df", "data.frame")

  structure(collections, class = class)

}

#' @noRd

get_collections <- function(col_obj) {

  nms <- col_obj[["nms"]]

  qry <- col_obj[["qry"]]

  path <- col_obj[["path"]]

  cache <- col_obj[["cache"]]

  page <- 0L

  page_size <- 1000L

  page_args <- list(page = page, pageSize = page_size)

  qry <- c(qry, page_args)

  collections_list <- list()

  cond <- TRUE

  while (cond) {

    page <- page + 1L

    qry[["page"]] <- page

    query_obj <- list(path = path, query = qry, cache = cache)

    resp <- api_get(query_obj)

    collections_list[[page]] <- resp

    content <- resp[["content"]]

    total <- content[["total"]]

    cond <- total > page * page_size

  }

  elements <- c("content", "results")

  for (i in elements) {

    collections_list <- lapply(collections_list, getElement, i)

  }

  collections_list <- do.call(c, collections_list)

  collections_seq <- seq_along(collections_list)

  for (i in collections_seq) {

    collections_i <- collections_list[[i]]

    for (nm in nms) {

      ans <- collections_i[[nm]]

      ans_is_null <- is.null(ans)

      if (ans_is_null) {

        ans <- NA

      }

      collections_i[[nm]] <- ans

    }

    collections_list[[i]] <- collections_i

  }

  collections <- list()

  for (nm in nms) {

    collections_nm <- lapply(collections_list, getElement, nm)

    collections[[nm]] <- collections_nm

  }

  lth_of_els <- lapply(collections, lapply, length)

  lth_of_els <- lapply(lth_of_els, unlist)

  lth_of_els <- vapply(lth_of_els, max, 0L)

  mt_one_el <- lth_of_els > 1L

  nms <- split(nms, mt_one_el)

  nms_mt_one_el <- nms[["TRUE"]]

  list_cols <- collections[nms_mt_one_el, drop = FALSE]

  nms_one_el <- nms[["FALSE"]]

  one_el_cols <- collections[nms_one_el, drop = FALSE]

  collections <- lapply(one_el_cols, unlist)

  collections <- as.data.frame(
    collections, col.names = nms_one_el, stringsAsFactors = FALSE
  )

  collections[nms_mt_one_el] <- list_cols

  id <- col_obj[["id"]]

  collections_id <- collections[[id]]

  collections_id <- gsub("^http:\\/\\/tun\\.fi\\/", "", collections_id)

  collections[[id]] <- collections_id

  col_names <- names(collections)

  col_names <- sub("\\.", "_", col_names)

  col_names <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", col_names, perl = TRUE)

  structure(collections, names = col_names)

}

#' @noRd
#' @importFrom digest digest
#' @importFrom httr RETRY

get_swagger <- function(cache) {

  fb_api_url <- getOption("finbif_api_url")

  url <- paste0(fb_api_url, "/explorer/swagger.json")

  timeout <- cache

  cache_logical <- is.logical(cache)

  if (cache_logical) {

    timeout <- Inf

  } else {

    cache <- cache > 0

  }

  if (cache) {

    hash <- digest::digest(url)

    fcp <- getOption("finbif_cache_path")

    fcp_is_null <- is.null(fcp)

    if (fcp_is_null) {

      cache <- get_cache(hash)

      has_cache <- !is.null(cache)

      if (has_cache) {

        return(cache)

      }

      on.exit({

        cache_obj <- list(data = ans, hash = hash, timeout = timeout)

        set_cache(cache_obj)

      })

    } else {

      cache_file_name <- paste0("finbif_cache_file_", hash)

      cache_file_path <- file.path(fcp, cache_file_name)

      cache_file_exists <- file.exists(cache_file_path)

      if (cache_file_exists) {

        created <- file.mtime(cache_file_path)

        timeout <- timeout * 3600

        current <- Sys.time()

        elapsed <- current - created

        valid <- timeout > elapsed

        if (valid) {

          cached_obj <- readRDS(cache_file_path)

          return(cached_obj)

        } else {

          unlink(cache_file_path)

        }

      }

      on.exit({

        saveRDS(ans, cache_file_path)

      })

    }

  }

  allow <- getOption("finbif_allow_query")

  stopifnot("Request not cached and option:finbif_allow_query = FALSE" = allow)

  rate_limit <- getOption("finbif_rate_limit")

  sleep <- 1 / rate_limit

  Sys.sleep(sleep)

  times <- getOption("finbif_retry_times")

  pause_base <- getOption("finbif_retry_pause_base")

  pause_cap <- getOption("finbif_retry_pause_cap")

  pause_min <- getOption("finbif_retry_pause_min")

  ans <- httr::RETRY("GET",
    url,
    times = times,
    pause_base = pause_base,
    pause_cap = pause_cap,
    pause_min = pause_min,
    terminate_on = 404L
  )

  ans

}
