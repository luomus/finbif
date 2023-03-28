# records ----------------------------------------------------------------------

#' Get FinBIF records
#'
#' Download records from FinBIF. The function `finbif_records()` and its
#' associated classes and methods have been deprecated and user access will be
#' removed in the next release of the finbif package.
#'
#' @aliases fb_records
#'
#' @param filter List of named character vectors. Filters to apply to records.
#' @param select Character vector. Variables to return. If not specified, a
#'   default set of commonly used variables will be used. Use `"default_vars"`
#'   as a shortcut for this set. Variables can be deselected by prepending a `-`
#'   to the variable name. If only deselects are specified the default set of
#'   variables without the deselection will be returned.
#' @param order_by Character vector. Variables to order records by before they
#'   are returned. Most, though not all, variables can be used to order records
#'   before they are returned. Ordering is ascending by default. To return in
#'   descending order append a `-` to the front of the variable (e.g.,
#'   `"-date_start"`). Default order is `"-date_start"` > `"-load_data"` >
#'   `"reported_name"`.
#' @param aggregate Character. If `"none"` (default), returns full records. If
#'   one or more of `"records"`, `"species"`, `"taxa"`, `"individuals"`,
#'   `"pairs"`, `"events"` or `"documents"`; aggregates combinations of the
#'   selected variables by counting records, species, taxa, indivduals or events
#'   or documents. Aggregation by events or documents cannot be done in
#'   combination with any of the other aggregation types.
#' @param sample Logical. If `TRUE` randomly sample the records from the FinBIF
#'   database.
#' @param n Integer. How many records to download/import.
#' @param page Integer. Which page of records to start downloading from.
#' @param count_only Logical. Only return the number of records available.
#' @param quiet Logical. Suppress the progress indicator for multipage
#'   downloads. Defaults to value of option `finbif_hide_progress`.
#' @param cache Logical. Use cached data.
#' @param dwc Logical. Use Darwin Core (or Darwin Core style) variable names.
#' @param seed Integer. Set a seed for randomly sampling records.
#' @param df Logical. Should the data.frame representation of the records be
#'   returned as an attribute?
#' @param exclude_na Logical. Should records where all selected variables have
#'   non-NA values only be returned.
#' @param locale Character. One of the supported two-letter ISO 639-1 language
#'   codes. Current supported languages are English, Finnish and Swedish. For
#'   data where more than one language is available the language denoted by
#'   `locale` will be preferred while falling back to the other languages in the
#'   order indicated above.
#' @param include_facts Logical. Should all "fact" variables be included?
#' @return A `finbif_api` or `finbif_api_list` object.
#' @examples \dontrun{
#'
#' # Get the last 100 records from FinBIF
#' finbif_records(n = 100)
#'
#' }
#' @export

finbif_records <- function(
  filter = NULL,
  select = NULL,
  order_by = NULL,
  aggregate = "none",
  sample = FALSE,
  n = 10,
  page = 1,
  count_only = FALSE,
  quiet = getOption("finbif_hide_progress"),
  cache = getOption("finbif_use_cache"),
  dwc = FALSE,
  seed = NULL,
  df = FALSE,
  exclude_na = FALSE,
  locale = getOption("finbif_locale"),
  include_facts = FALSE
) {

  msg <- paste0(
    "finbif_records() and its associated classes and methods have been ",
    "deprecated and user access will be removed in the next release of the ",
    "finbif package."
  )

  deprecation(msg)

  fb_records_obj <- list(
    filter = filter,
    select = select,
    order_by = order_by,
    aggregate = aggregate,
    sample = sample,
    n = n,
    page = page,
    count_only = count_only,
    quiet = quiet,
    cache = cache,
    dwc = dwc,
    seed = seed,
    df = df,
    exclude_na = exclude_na,
    locale = locale,
    include_facts = include_facts
  )

  records(fb_records_obj)

}

#' @noRd

records <- function(fb_records_obj) {

  aggregate <- fb_records_obj[["aggregate"]]

  filter <- fb_records_obj[["filter"]]

  order_by <- fb_records_obj[["order_by"]]

  var_type <- fb_records_obj[["var_type"]]

  seed <- fb_records_obj[["seed"]]

  sample <- fb_records_obj[["sample"]]

  df <- fb_records_obj[["df"]]

  count_only <- fb_records_obj[["count_only"]]

  locale <- fb_records_obj[["locale"]]

  max_size <- getOption("finbif_max_page_size")

  fb_records_obj[["max_size"]] <- max_size

  max_queries <- getOption("finbif_max_queries")

  nmax <- max_queries * max_size

  fb_records_obj[["nmax"]] <- nmax

  dwc <- fb_records_obj[["dwc"]]

  var_type <- col_type_string(dwc)

  fb_records_obj[["var_type"]] <- var_type

  n <- fb_records_obj[["n"]]

  n <- as.integer(n)

  fb_records_obj[["n"]] <- n

  defer_errors({

    check_n(fb_records_obj)

    # aggregation ==============================================================

    aggregate <- infer_aggregation(aggregate)

    fb_records_obj[["aggregate"]] <- aggregate

    # filter ===================================================================

    query <- list()

    has_filter <- !is.null(filter)

    if (has_filter) {

      parsed_filters <- parse_filters(fb_records_obj)

      query <- lapply(parsed_filters, paste, collapse = ",")

    }

    # select ===================================================================

    select <- infer_selection(fb_records_obj)

    select_query <- select[["query"]]

    fb_records_obj[["select_query"]] <- select_query

    select_user <- select[["user"]]

    fb_records_obj[["select_user"]] <- select_user

    record_id_selected <- select[["record_id_selected"]]

    fb_records_obj[["record_id_selected"]] <- record_id_selected

    date_time_selected <- select[["date_time_selected"]]

    fb_records_obj[["date_time_selected"]] <- date_time_selected

    aggregate <- aggregate[[1L]]

    select_param <- switch(aggregate, none = "selected", "aggregateBy")

    fb_records_obj[["select_param"]] <- select_param

    query[[select_param]] <- paste(select_query, collapse = ",")

    # order ====================================================================

    has_order <- !is.null(order_by)

    if (has_order) {

      desc_order <- grepl("^-", order_by)

      order_by <- sub("^-", "", order_by)

      var_names <- var_names()

      var_names_order <- var_names[["order"]]

      order_vars <- var_names[var_names_order, var_type, drop = FALSE]

      order_vars[] <- lapply(order_vars, structure, class = "translation")

      order_vars <- list(order_vars = order_vars)

      order_vars <- list(
        x = order_by, translation = "order_vars", env = order_vars
      )

      order_by <- translate(order_vars)

      order_by <- order_by_computed_var(order_by)

      order_by_desc <- order_by[desc_order]

      order_by_desc <- paste(order_by_desc, "DESC")

      order_by[desc_order] <- order_by_desc

      order_by <- paste(order_by, collapse = ",")

    }

    if (sample) {

      seed <- as.integer(seed)

      seed <- c("RANDOM", seed)

      seed <- paste(seed, collapse = ":")

      order_by <- c(seed, order_by)

      order_by <- paste(order_by, collapse = ",")

    }

    query[["orderBy"]] <- order_by

  })

  fb_records_obj[["query"]] <- query

  fb_records_obj <- na_exclude(fb_records_obj)

  ans <- request(fb_records_obj)

  df <- df && !count_only

  if (df) {

    ind <- length(ans)

    last <- ans[[ind]]

    last_df <- as.data.frame(last, locale = locale)

    attr(last, "df") <- last_df

    ans[[ind]] <- last

  }

  ans

}

# aggregation ------------------------------------------------------------------

#' @noRd

infer_aggregation <- function(aggregate) {

  events_and_docs <- c("events", "documents")

  aggregations <- c(
    "none", "records", "species", "taxa", "individuals", "pairs",
    events_and_docs
  )

  aggregate <- match.arg(aggregate, aggregations, TRUE)

  has_events_or_docs <- events_and_docs %in% aggregate

  l <- length(aggregate)

  more_than_one <- l > 1L

  cond <- more_than_one && any(has_events_or_docs)

  if (cond) {

    deferrable_error(
      "Chosen aggregation cannot by combined with other aggregations"
    )
  }

  aggregate

}

# selection --------------------------------------------------------------------

#' @noRd

infer_selection <- function(fb_records_obj) {

  aggregate <- fb_records_obj[["aggregate"]]

  aggregate <- aggregate[[1L]]

  select <- fb_records_obj[["select"]]

  include_facts <- fb_records_obj[["include_facts"]]

  var_type <- fb_records_obj[["var_type"]]

  var_names <- var_names()

  is_date_time_vars <- var_names[["date"]]

  date_time_vars <- var_names[is_date_time_vars, ]

  date_time_var_names <- row.names(date_time_vars)

  is_default_vars <- var_names[["default_var"]]

  default_vars <- switch(
    aggregate,
    none = var_names[is_default_vars, ],
    events = var_names["gathering.gatheringId", ],
    documents = var_names["document.documentId", ],
    var_names["unit.linkings.taxon.scientificName", ]
  )

  select_null <- is.null(select)

  aggregate_none <- identical(aggregate, "none")

  if (select_null) {

    select <- row.names(default_vars)

    select_user <- default_vars[[var_type]]

    record_id_selected <- FALSE

    date_time_selected <- FALSE

    if (aggregate_none) {

      # Missing 'select' implies default selection which implies date-time,
      # abundance, coord uncertainty and scientific name calc needed

      select <- c(
        select,
        date_time_var_names,
        "unit.interpretations.individualCount",
        "unit.abundanceString",
        "gathering.interpretations.coordinateAccuracy",
        "unit.linkings.taxon.scientificName",
        "unit.taxonVerbatim",
        "unit.linkings.taxon.scientificNameAuthorship",
        "unit.author",
        "document.sourceId"
      )

      select <- unique(select)

      record_id_selected <- TRUE

      date_time_selected <- TRUE

    }

  } else {

    deselect <- grep("^-", select, value = TRUE)

    deselect <- substring(deselect, 2L)

    n_deselect <- length(deselect)

    n_select <- length(select)

    all_deselect <- identical(n_deselect, n_select)

    if (all_deselect) {

      select <- "default_vars"

    }

    select <- grep("^-", select, value = TRUE, invert = TRUE)

    default_vars <- default_vars[[var_type]]

    default_vars <- list(default_vars)

    select <- ifelse(select == "default_vars", default_vars, select)

    select <- unlist(select)

    select_ind <- !select %in% deselect

    select <- select[select_ind]

    select_user <- select

    select <- unique(select)

    record_id <- var_names["unit.unitId", var_type]

    record_id_selected <- record_id %in% select

    needs_record_id <- !record_id_selected && aggregate_none

    if (needs_record_id) {

      select <- c(record_id, select)

    }

    date_time_var_translations <- c(
      "date_time",
      "eventDateTime",
      "date_time_ISO8601",
      "eventDate",
      "duration",
      "samplingEffort"
    )

    date_time_selected <- date_time_var_translations %in% select

    date_time_selected <- any(date_time_selected)

    if (date_time_selected) {

      date_time_vars_selection <- date_time_vars[[var_type]]

      select <- c(select, date_time_vars_selection)

    }

    fb_records_obj[["select"]] <- select

    select <- infer_computed_vars(fb_records_obj)

    select_type <- switch(
      aggregate,
      none = "select",
      events = "aggregate_events",
      documents = "aggregate_documents",
      "aggregate"
    )

    select_type_rows <- var_names[[select_type]]

    select_vars <- var_names[select_type_rows, var_type, drop = FALSE]

    select_vars[] <- lapply(select_vars, structure, class = "translation")

    select_vars <- list(select_vars = select_vars)

    select_vars <- list(
      x = select, translation = "select_vars", env = select_vars
    )

    select <- translate(select_vars)

    vars_computed_from_id <- grepl("^computed_var_from_id", select)

    any_vars_computed_from_id <- any(vars_computed_from_id)

    if (any_vars_computed_from_id) {

      select_computed <- select[vars_computed_from_id]

      vars_computed_from_id <- var_names[select_computed, ]

      n_computed <- nrow(vars_computed_from_id)

      computed_sq <- seq_len(n_computed)

      for (i in computed_sq) {

        computed_names <- row.names(vars_computed_from_id)

        computed_name_i <- computed_names[[i]]

        ind <- match(computed_name_i, select)

        computed_var <- vars_computed_from_id[i, var_type]

        suffix <- switch(var_type, translated_var = "_id", dwc = "ID")

        id_var <- paste0(computed_var, suffix)

        select_vars[["x"]] <- id_var

        select_i <- translate(select_vars)

        select[[ind]] <- select_i

      }

    }

  }

  if (include_facts) {

    select <- c(
      select,
      "unit.facts.fact",
      "unit.facts.value",
      "gathering.facts.fact",
      "gathering.facts.value",
      "document.facts.fact",
      "document.facts.value"
    )

  }

  # Can't query the server for vars that are computed after download

  uncomputed <- !grepl("^computed_var", select)

  select <- select[uncomputed]

  select <- unique(select)

  list(
    query = select,
    user = select_user,
    record_id_selected = record_id_selected,
    date_time_selected = date_time_selected
  )

}

#' @noRd

infer_computed_vars <- function(fb_records_obj) {

  select <- fb_records_obj[["select"]]

  var_type <- fb_records_obj[["var_type"]]

  abundance_vars <- c(
    "abundance",
    "individualCount",
    "occurrence_status",
    "occurrenceStatus"
  )

  abundance_var_names <- c(
    "unit.interpretations.individualCount",
    "unit.abundanceString"
  )

  abundance <- list(vars = abundance_vars, v_names = abundance_var_names)

  cu_vars <- c(
    "coordinates_uncertainty",
    "coordinateUncertaintyInMeters"
  )

  cu_var_names <- c(
    "gathering.interpretations.coordinateAccuracy",
    "document.sourceId"
  )

  cu <- list(vars = cu_vars, v_names = cu_var_names)

  citation_vars <- c(
    "citation",
    "bibliographicCitation"
  )

  citation_var_names <- c(
    "document.documentId",
    "document.sourceId"
  )

  citation <- list(vars = citation_vars, v_names = citation_var_names)

  sn_vars <- c(
    "scientific_name",
    "scientificName"
  )

  sn_var_names <- c(
    "unit.linkings.taxon.scientificName",
    "unit.taxonVerbatim",
    "unit.linkings.taxon.scientificNameAuthorship",
    "unit.author",
    "document.sourceId"
  )

  sn <- list(vars = sn_vars, v_names = sn_var_names)

  red_list_vars <- c(
    "red_list_status",
    "redListStatus"
  )

  red_list_var_names <- c(
    "unit.linkings.taxon.latestRedListStatusFinland.status",
    "unit.linkings.taxon.latestRedListStatusFinland.year"
  )

  red_list <- list(vars = red_list_vars, v_names = red_list_var_names)

  region_vars <- c(
    "region",
    "stateProvince"
  )

  region_var_names <- c(
    "gathering.interpretations.finnishMunicipality"
  )

  region <- list(vars = region_vars, v_names = region_var_names)

  computed_var_list <- list(abundance, cu, citation, sn, red_list, region)

  var_names <- var_names()

  for (i in computed_var_list) {

    vars_i <- i[["vars"]]

    v_names_i <- i[["v_names"]]

    cond <- vars_i %in% select

    cond <- any(cond)

    if (cond) {

      inferred <- var_names[v_names_i, var_type]

      select <- c(select, inferred)

    }

  }

  select

}

# request ----------------------------------------------------------------------

#' @noRd

request <- function(fb_records_obj) {

  filter <- fb_records_obj[["filter"]]

  select <- fb_records_obj[["select_query"]]

  sample <- fb_records_obj[["sample"]]

  n <- fb_records_obj[["n"]]

  page <- fb_records_obj[["page"]]

  count_only <- fb_records_obj[["count_only"]]

  quiet <- fb_records_obj[["quiet"]]

  cache <- fb_records_obj[["cache"]]

  query <- fb_records_obj[["query"]]

  max_size <- fb_records_obj[["max_size"]]

  select_user <- fb_records_obj[["select_user"]]

  record_id_selected <- fb_records_obj[["record_id_selected"]]

  date_time_selected <- fb_records_obj[["date_time_selected"]]

  dwc <- fb_records_obj[["dwc"]]

  aggregate <- fb_records_obj[["aggregate"]]

  df <- fb_records_obj[["df"]]

  seed <- fb_records_obj[["seed"]]

  exclude_na <- fb_records_obj[["exclude_na"]]

  locale <- fb_records_obj[["locale"]]

  include_facts <- fb_records_obj[["include_facts"]]

  path <- getOption("finbif_warehouse_query")

  count_records <- count_only && identical(aggregate, "none")

  if (count_records) {

    query[["selected"]] <- NULL

    query[["orderBy"]]  <- NULL

    path <- paste0(path, "unit/count")

    request_obj <- list(path = path, query = query, cache = cache)

    resp <- api_get(request_obj)

    return(resp)

  }

  endpoint <- select_endpoint(fb_records_obj)

  path <- paste0(path, endpoint)

  fb_records_obj[["path"]] <- path

  if (count_only) {

    query[["page"]] <- 1L

    query[["pageSize"]] <- 1L

    request_obj <- list(path = path, query = query, cache = cache)

    resp <- api_get(request_obj)

    n_tot <- resp[["content"]]

    n_tot <- n_tot[["total"]]

    n_tot <- list(total = n_tot)

    resp[["content"]] <- n_tot

    return(resp)

  }

  taxon_counts <- taxa_counts(fb_records_obj)

  query[["taxonCounts"]] <- taxon_counts

  individual_counts <- individual_counts(fb_records_obj)

  query[["onlyCount"]] <- individual_counts

  pair_counts <- pair_counts(fb_records_obj)

  query[["pairCounts"]] <- pair_counts

  query[["page"]] <- page

  page_size <- min(n, max_size)

  query[["pageSize"]] <- page_size

  fb_records_obj[["query"]] <- query

  resp <- records_obj(fb_records_obj)

  n_tot <- resp[["content"]]

  n_tot <- n_tot[["total"]]

  n <- min(n, n_tot)

  resp <- list(resp)

  class <- c("finbif_records_list", "finbif_api_list")

  select <- unique(select)

  fb_records_list <- structure(
    resp,
    class = class,
    max_size = max_size,
    quiet = quiet,
    path = path,
    filter = filter,
    query = query,
    nrec_dnld = n,
    nrec_avl = n_tot,
    seed = seed,
    select = select,
    select_user = select_user,
    locale = locale,
    df = df,
    dwc = dwc,
    exclude_na = exclude_na,
    include_facts = include_facts,
    count_only = count_only,
    record_id = record_id_selected,
    date_time = date_time_selected,
    aggregate = aggregate,
    cache = cache
  )

  need_more_pages <- n > max_size

  if (need_more_pages) {

    # If random sampling and requesting few records or a large proportion of the
    # total number of records, it makes more sense to just get all the records
    # and sample afterwards to avoid coping with duplicates due to pagination.

    sample_after_request <-  sample && sample_after(fb_records_list)

    if (sample_after_request) {

      fb_records_obj[["select"]] <- select_user

      fb_records_obj[["sample"]] <- FALSE

      fb_records_obj[["n"]] <- n_tot

      fb_records_list <- records(fb_records_obj)

      attr(fb_records_list, "nrec_dnld") <- n

      sampled_records <- record_sample(fb_records_list)

      return(sampled_records)

    }

    fb_records_list <- get_extra_pages(fb_records_list)

    if (sample) {

      no_seed <- is.null(seed)

      if (no_seed) {

        attr(fb_records_list, "seed") <- 1L

      }

      fb_records_list <- handle_duplicates(fb_records_list)

    }

  }

  fb_records_list

}

#' @noRd

sample_after <- function(fb_records_list) {

  n <- attr(fb_records_list, "nrec_dnld", TRUE)

  n_tot <- attr(fb_records_list, "nrec_avl", TRUE)

  ratio <- n / n_tot

  cond <- ratio > .5

  cond || gt_max_size3(n_tot)

}

#' @noRd

gt_max_size3 <- function(n) {

  max_size <- getOption("finbif_max_page_size")

  max_size3 <- max_size * 3L

  n < max_size3

}

# construct request ------------------------------------------------------------

#' @noRd

records_obj <- function(fb_records_obj) {

  response <- api_get(fb_records_obj)

  class <- c("finbif_records", "finbif_api")

  select <- fb_records_obj[["select_query"]]

  select <- unique(select)

  aggregate <- fb_records_obj[["aggregate"]]

  structure(response, class = class, select = select, aggregate = aggregate)

}

# record pagination ------------------------------------------------------------

#' @noRd
#' @importFrom utils txtProgressBar setTxtProgressBar

get_extra_pages <- function(fb_records_list) {

  n <- attr(fb_records_list, "nrec_dnld", TRUE)

  max_size <- attr(fb_records_list, "max_size", TRUE)

  quiet <- attr(fb_records_list, "quiet", TRUE)

  path <- attr(fb_records_list, "path", TRUE)

  query <- attr(fb_records_list, "query", TRUE)

  cache <- attr(fb_records_list, "cache", TRUE)

  select <- attr(fb_records_list, "select", TRUE)

  aggregate <- attr(fb_records_list, "aggregate", TRUE)

  df <- attr(fb_records_list, "df", TRUE)

  locale <- attr(fb_records_list, "locale", TRUE)

  fb_records_obj <- list(
    path = path, cache = cache, select_query = select, aggregate = aggregate
  )

  multipage <- n > max_size

  use_pb <- multipage && !quiet

  if (use_pb) {

    pb_head("Fetching data")

    ratio <- n / max_size

    max <- floor(ratio)

    pb <- utils::txtProgressBar(0L, max, style = 3L)

    on.exit({

      close(pb)

    })

  }

  i <- 1L

  page <- query[["page"]]

  page <- page + 1L

  page_size <- query[["pageSize"]]

  n_pages <- n %/% page_size

  has_future <- has_pkgs("future")

  if (has_future) {

    value <- future::value

  }

  while (multipage) {

    if (!quiet) {

      utils::setTxtProgressBar(pb, i)

    }

    no_more_pages <- page > n_pages

    if (no_more_pages) {

      excess_records <- n %% page_size

      last_record <- page_size * n_pages

      no_more_records <- identical(last_record, n)

      if (no_more_records) {

        break

      }

      page_size <- get_next_lowest_factor(last_record, excess_records)

      page <- last_record / page_size

      page <- page + 1L

      n_pages <- n %/% page_size

    }

    query[["page"]] <- page

    query[["pageSize"]] <- page_size

    fb_records_obj[["query"]] <- query

    delayedAssign("res", records_obj(fb_records_obj))

    if (has_future) {

      res <- future::future(
        {

          records_obj(fb_records_obj)

        },
        seed = NULL
      )

    }

    if (df) {

      fb_records_list_i <- fb_records_list[[i]]

      # More performant to convert to data.frame directly from list

      fb_records_df_i <- as.data.frame(fb_records_list[[i]], locale = locale)

      attr(fb_records_list_i, "df") <- fb_records_df_i

      fb_records_list[[i]] <- fb_records_list_i

    }

    i <- i + 1L

    res <- value(res)

    fb_records_list[[i]] <- res

    page <- page + 1L

  }

  fb_records_list

}

# parsing filters --------------------------------------------------------------

#' @noRd

parse_filters <- function(fb_records_obj) {

  filter <- fb_records_obj[["filter"]]

  aggregate <- fb_records_obj[["aggregate"]]

  locale <- fb_records_obj[["locale"]]

  filter <- as.list(filter)

  finbif_filter_names <- names(filter)

  finbif_filter_names <- list(
    x = finbif_filter_names, translation = "filter_names", env = -1
  )

  finbif_filter_names <- translate(finbif_filter_names)

  aggregate <- aggregate[[1L]]

  taxon_filters <- c("taxonId", "target")

  is_taxon_filter <- finbif_filter_names %in% taxon_filters

  cond <- any(is_taxon_filter)

  cond <- cond && switch(aggregate, events = TRUE, documents = TRUE, FALSE)

  if (cond) {

    deferrable_error("Cannot use current aggregation and filter by taxon")

  }

  filter_sq <- seq_along(filter)

  cols <- c("id", "collection_name", "abbreviation")

  filter_names <- filter_names()

  for (i in filter_sq) {

    filter_name_i <- finbif_filter_names[[i]]

    # the filter might not exist

    name_na <- is.na(filter_name_i)

    if (name_na) {

      next

    }

    nms <- names(filter)

    nm_i <- nms[[i]]

    filter_i <- filter[[i]]

    requires_translation <- filter_names[[filter_name_i, "translated_values"]]

    if (requires_translation) {

      filter_i <- list(x = filter_i, translation = nm_i, env = -1)

      filter_i <- translate(filter_i)

    }

    is_collection_filter <- grepl("^(not_){0,1}collection$", nm_i) # nolint

    if (is_collection_filter) {

      is_collection <- inherits(filter_i, "finbif_collections")

      if (is_collection) {

        filter_i <- row.names(filter_i)

      } else {

        env <- list()

        collections <- finbif_collections(
          select = cols, supercollections = TRUE, nmin = NA, locale = locale
        )

        collections[] <- lapply(collections, structure, class = "translation")

        env[[nm_i]] <- collections

        filter_i <- list(x = filter_i, translation = nm_i, env = env)

        filter_i <- translate(filter_i)

      }

    }

    class <- filter_names[[filter_name_i, "class"]]

    is_coords <- identical(class, "coords")

    if (is_coords) {

      # Coordinates filter must have a system defined

      coordinates_filter <- filter[["coordinates"]]

      coordinates_obj <- list(name = filter_name_i, filter = coordinates_filter)

      check_coordinates(coordinates_obj)

      filter_i <- coords(filter_i)

    }

    is_date <- identical(class, "date")

    if (is_date) {

      date_filter <- list(filter = nm_i)

      date_filter <- c(date_filter, filter_i)

      filter_i <- dates(date_filter)

    }

    sep <- filter_names[filter_name_i, "sep"]

    filter_i <- paste(filter_i, collapse = sep)

    filter[[i]] <- filter_i

  }

  names(filter) <- finbif_filter_names

  filter

}

#' @noRd

check_coordinates <- function(obj) {

  name <- obj[["name"]]

  filter <- obj[["filter"]]

  n_filters <- length(filter)

  nms <- names(filter)

  empty_names <- nms == ""

  has_names <- !is.null(nms)

  cond <- has_names && !empty_names[[3L]]

  cond <- cond && !"system" %in% nms

  cond <- cond || n_filters < 3L

  cond <- cond && identical(name, "coordinates")

  if (cond) {

    deferrable_error("Invalid coordinates: system not specified")

  }

}

# translation ------------------------------------------------------------------

#' @noRd

translate <- function(translation_obj) {

  x <- translation_obj[["x"]]

  translation <- translation_obj[["translation"]]

  pos <- translation_obj[["env"]]

  trsltn <- get(translation, pos)

  is_fun <- is.function(trsltn)

  if (is_fun) {

    trsltn <- trsltn()

  }

  # Some filters have multi-level values to translate (e.g., primary_habitat)

  is_list <- is.list(x)

  if (is_list) {

    nms <- names(x)

    translation_names <- names(trsltn)

    translation_names1 <- translation_names[[1L]]

    nms <- list(x = nms, translation = translation_names1, env = trsltn)

    nms <- translate(nms)

    names(x) <- nms

    translation_names2 <- translation_names[[2L]]

    x <- lapply(x, list, translation_names2, trsltn)

    translation_obj_names <- c("x", "translation", "env")

    x <- lapply(x, structure, names = translation_obj_names)

    x <- lapply(x, translate)

    x <- lapply(x, paste, collapse = ",")

    nms <- names(x)

    empty <- x == ""

    unempty_x <- sprintf("[%s]", x)

    x <- ifelse(empty, x, unempty_x)

    sprintf("%s%s", nms, x)

  } else {

    n <- length(x)

    ind <- rep(NA_integer_, n)

    # multilevel filters have data.frames a level below

    not_df <- !is.data.frame(trsltn)

    if (not_df) {

      trsltn <- trsltn[[1L]]

    }

    for (i in trsltn) {

      is_translation <- inherits(i, "translation")

      if (is_translation) {

        x_low <- tolower(x)

        i_low <- tolower(i)

        matched <- match(x_low, i_low)

        matched_na <- is.na(matched)

        ind <- ifelse(matched_na, ind, matched)

      }

    }

    any_na <- anyNA(ind)

    if (any_na) {

      na_ind <- is.na(ind)

      x_na <- x[na_ind]

      for (err in x_na) {

        translation <- gsub("_", " ", translation)

        invalid <- paste0("Invalid name in ", translation, ": ", err)

        deferrable_error(invalid)

      }

    }

    ans <- row.names(trsltn)

    ans <- ans[ind]

    unique <- !grepl("DUPLICATE", ans)

    ans[unique]

  }

}

# sample records ---------------------------------------------------------------

#'@noRd

record_sample <- function(fb_records_list) {

  n  <- attr(fb_records_list, "nrec_dnld", TRUE)

  n_tot <- attr(fb_records_list, "nrec_avl", TRUE)

  select <- attr(fb_records_list, "select", TRUE)

  record_id <- attr(fb_records_list, "record_id", TRUE)

  cache <- attr(fb_records_list, "cache", TRUE)

  size <- n_tot - n

  remove <- sample.int(n_tot, size)

  if (cache) {

    seed <- gen_seed(fb_records_list)

    remove <- sample_with_seed(n_tot, size, seed)

  }

  attr(fb_records_list, "remove") <- remove

  class <- c(
    "finbif_records_sample_list", "finbif_records_list", "finbif_api_list"
  )

  ans <- remove_records(fb_records_list)

  structure(
    ans,
    class = class,
    nrec_dnld = n,
    nrec_avl = n_tot,
    select = select,
    record_id = record_id,
    cache = cache
  )

}

# handle duplicates ------------------------------------------------------------

#' @noRd

handle_duplicates <- function(fb_records_list) {

  filter <- attr(fb_records_list, "filter", TRUE)

  select <- attr(fb_records_list, "select_user", TRUE)

  max_size <- attr(fb_records_list, "max_size", TRUE)

  cache <- attr(fb_records_list, "cache", TRUE)

  n <- attr(fb_records_list, "nrec_dnld", TRUE)

  seed <- attr(fb_records_list, "seed", TRUE)

  dwc <- attr(fb_records_list, "dwc", TRUE)

  df <- attr(fb_records_list, "df", TRUE)

  exclude_na <- attr(fb_records_list, "exclude_na", TRUE)

  locale <- attr(fb_records_list, "locale", TRUE)

  include_facts <- attr(fb_records_list, "include_facts", TRUE)

  count_only <- attr(fb_records_list, "count_only", TRUE)

  ids <- lapply(fb_records_list, extract_ids)

  ids <- unlist(ids)

  duplicates <- duplicated(ids)

  duplicates <- which(duplicates)

  attr(fb_records_list, "remove") <- duplicates

  fb_records_list <- remove_records(fb_records_list)

  n_id <- length(ids)

  n_dups <-  length(duplicates)

  n_unique <- n_id - n_dups

  need_new_records <- n_unique < n

  if (need_new_records) {

    fb_records_obj <- list(
      filter = filter,
      select = select,
      sample = TRUE,
      n = max_size,
      cache = cache,
      dwc = dwc,
      seed = seed,
      df = df,
      exclude_na = exclude_na,
      locale = locale,
      include_facts = include_facts,
      count_only = count_only
    )

    new_records <- records(fb_records_obj)

    new_records <- new_records[[1L]]

    n_records <- length(fb_records_list)

    next_records <- n_records + 1L

    fb_records_list[[next_records]] <- new_records

    fb_records_list <- handle_duplicates(fb_records_list)

  }

  attr(fb_records_list, "remove") <- NULL

  remove_records(fb_records_list)

}

#' @noRd

extract_ids <- function(x) {

  unit_id <- c("unit", "unitId")

  x <- x[["content"]]

  x <- x[["results"]]

  vapply(x, get_el_recurse, "", unit_id, "character")

}

# remove records ---------------------------------------------------------------

#' @noRd

remove_records <- function(fb_records_list) {

  n <- attr(fb_records_list, "nrec_dnld", TRUE)

  remove <- attr(fb_records_list, "remove", TRUE)

  contents <- lapply(fb_records_list, getElement, "content")

  page_sizes <- vapply(contents, getElement, 0L, "pageSize")

  records_null <- is.null(remove)

  if (records_null) {

    total_page_sizes <- sum(page_sizes)

    remove <- seq(1L, total_page_sizes)

    which_records <- seq(1L, n)

    remove <- remove[-which_records]

  }

  sq <- seq_along(fb_records_list)

  excess_pages <- rep.int(sq, page_sizes)

  excess_pages <- excess_pages[remove]

  nl <- length(fb_records_list)

  page_size_sum <- cumsum(page_sizes)

  page_size_sum <- page_size_sum[-nl]

  records <- c(0L, page_size_sum)

  records <- records[excess_pages]

  records <- remove - records

  records <- split(records, excess_pages)

  for (i in sq) {

    char_i <- as.character(i)

    ind <- records[[char_i]]

    fb_records_list_i <- fb_records_list[[i]]

    content <- fb_records_list_i[["content"]]

    results <- content[["results"]]

    results[ind] <- NULL

    new_page_size <- length(results)

    content[["pageSize"]] <- new_page_size

    content[["results"]] <- results

    fb_records_list_i[["content"]] <- content

    has_ind <- !is.null(ind)

    if (has_ind) {

      df <- attr(fb_records_list_i, "df")

      df <- df[-ind, ]

      attr(fb_records_list_i, "df") <- df

    }

    fb_records_list[[i]] <- fb_records_list_i

  }

  contents <- lapply(fb_records_list, getElement, "content")

  page_sizes <- vapply(contents, getElement,  0L, "pageSize")

  to_remove <- page_sizes == 0L

  fb_records_list[to_remove] <- NULL

  fb_records_list

}

# utils ------------------------------------------------------------------------

#' @noRd

check_n <- function(fb_records_obj) {

  n <- fb_records_obj[["n"]]

  nmax <- fb_records_obj[["nmax"]]

  more_than_nmax <- n > nmax

  if (more_than_nmax) {

    msg <- paste("Cannot download more than", nmax, "records")

    deferrable_error(msg)

  }

  less_than_one <- n < 1L

  if (less_than_one) {

    deferrable_error("Cannot request less than 1 record")

  }

}

#' @noRd

select_endpoint <- function(fb_records_obj) {

  aggregate <- fb_records_obj[["aggregate"]]

  aggregate <- aggregate[[1L]]

  switch(
    aggregate,
    none = "unit/list",
    events = "gathering/aggregate",
    documents = "document/aggregate",
    "unit/aggregate"
  )

}

#' @noRd

taxa_counts <- function(fb_records_obj) {

  aggregate <- fb_records_obj[["aggregate"]]

  taxa_counts <- c("species", "taxa")

  has_taxa_count <- taxa_counts %in% aggregate

  without_taxa_counts <- !any(has_taxa_count)

  ans <- "true"

  if (without_taxa_counts) {

    ans <- NULL

  }

  ans

}

#' @noRd

individual_counts <- function(fb_records_obj) {

  aggregate <- fb_records_obj[["aggregate"]]

  without_individual_count <- !"individuals" %in% aggregate

  ans <- "false"

  if (without_individual_count) {

    ans <- NULL

  }

  ans

}

#' @noRd

pair_counts <- function(fb_records_obj) {

  aggregate <- fb_records_obj[["aggregate"]]

  without_pair_count <- !"pairs" %in% aggregate

  ans <- "true"

  if (without_pair_count) {

    ans <- NULL

  }

  ans

}

#' @noRd

order_by_computed_var <- function(order_by) {

  lt <- c(
    scientific_name = "unit.linkings.taxon.scientificName",
    abundance = "unit.interpretations.individualCount",
    date_time = "gathering.eventDate.begin",
    coordinates_uncertainty = "gathering.interpretations.coordinateAccuracy",
    red_list_status = "unit.linkings.taxon.redListStatus",
    citation = "document.documentId",
    occurrence_status = "unit.interpretations.individualCount",
    duration = "gathering.eventDate.begin",
    region = "gathering.province"
  )

  nms <- names(lt)

  nms <- paste0("computed_var_", nms)

  names(lt) <- nms

  ans <- lt[order_by]

  is_na <- is.na(ans)

  ans <- ifelse(is_na, order_by, ans)

  as.character(ans)

}

#' @noRd

na_exclude <- function(fb_records_obj) {

  query <- fb_records_obj[["query"]]

  exclude_na <- fb_records_obj[["exclude_na"]]

  select_param <- fb_records_obj[["select_param"]]

  if (exclude_na) {

    has_value <- query[[select_param]]

    has_value <- strsplit(has_value, ",")

    has_value <- c(hasValue = has_value)

    var_names <- var_names()

    available_vars <- row.names(var_names)

    ind <- var_names[["aggregate"]]

    ind <- ind | var_names[["aggregate_events"]]

    ind <- ind | var_names[["aggregate_documents"]]

    ind <- ind & var_names[["single"]]

    available_vars <- available_vars[ind]

    has_value <- lapply(has_value, intersect, available_vars)

    has_value <- lapply(has_value, paste, collapse = ",")

    query <- c(query, has_value)

  }

  fb_records_obj[["query"]] <- query

  fb_records_obj

}
