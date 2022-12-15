# records ----------------------------------------------------------------------

#' Get FinBIF records
#'
#' Download records from FinBIF.
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
#' @param aggregate Character. If missing, returns full records. If one or more
#'   of `"records"`, `"species"`, `"taxa"`, `"events"` or `"documents"`
#'   aggregates combinations of the selected variables by counting records,
#'   species, taxa, events or documents. Aggregation by events or documents
#'   cannot be done in combination with any of the other aggregation types.
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
#'   codes. Current supported languages are English, Finnish, Swedish, Russian,
#'   and Sámi (Northern). For data where more than one language is available
#'   the language denoted by `locale` will be preferred while falling back to
#'   the other languages in the order indicated above.
#' @param include_facts Logical. Should all "fact" variables be included?
#' @return A `finbif_api` or `finbif_api_list` object.
#' @examples \dontrun{
#'
#' # Get the last 100 records from FinBIF
#' finbif_records(n = 100)
#' }
#' @importFrom utils hasName txtProgressBar setTxtProgressBar
#' @export

finbif_records <- function(
  filter, select, order_by, aggregate, sample = FALSE, n = 10, page = 1,
  count_only = FALSE, quiet = getOption("finbif_hide_progress"),
  cache = getOption("finbif_use_cache"), dwc = FALSE, seed, df = FALSE,
  exclude_na = FALSE, locale = getOption("finbif_locale"), include_facts = FALSE
) {

  fb_records_obj <- list()

  max_size <- getOption("finbif_max_page_size")
  nmax     <- getOption("finbif_max_queries") * max_size
  n        <- as.integer(n)
  var_type <- col_type_string(dwc)

  defer_errors({

    fb_records_obj[["n"]] <- n
    fb_records_obj[["nmax"]] <- nmax

    check_n(fb_records_obj)

    # aggregation ==============================================================

    aggregate <- infer_aggregation(aggregate)

    # filter ===================================================================

    if (missing(filter)) {

      query <- list()

    } else {

      fb_records_obj[["filter"]] <- filter

      fb_records_obj[["aggregate"]] <- aggregate

      fb_records_obj[["locale"]] <- locale

      parsed_filters <- parse_filters(fb_records_obj)

      query <- lapply(parsed_filters, paste, collapse = ",")

    }

    # select ===================================================================

    select <- infer_selection(aggregate, select, include_facts, var_type)

    select_param <- switch(aggregate[[1L]], none = "selected", "aggregateBy")

    query[[select_param]] <- paste(select[["query"]], collapse = ",")

    # order ====================================================================

    if (missing(order_by)) {

      query[["orderBy"]] <- NULL

    } else {

      desc_order <- grepl("^-", order_by)
      order_by <- sub("^-", "", order_by)
      order_vars <- var_names[var_names[["order"]], var_type, drop = FALSE]
      class(order_vars[[var_type]]) <- class(var_names[[var_type]])
      order_by <-
        translate(order_by, "order_vars", list(order_vars = order_vars))
      order_by <- order_by_computed_var(order_by)
      order_by[desc_order] <- paste(order_by[desc_order], "DESC")
      query[["orderBy"]] <- paste(order_by, collapse = ",")

    }

    if (sample) {
      query[["orderBy"]] <- paste(
        if (missing(seed)) "RANDOM" else paste0("RANDOM:", as.integer(seed)),
        query[["orderBy"]],
        sep = c("", ",")[[length(query[["orderBy"]]) + 1L]]
      )
    }

  })

  query <- na_exclude(query, exclude_na, select_param)

  ans <- request(
    filter, select[["query"]], sample, n, page, count_only, quiet, cache, query,
    max_size, select[["user"]], select[["record_id_selected"]], dwc, aggregate,
    df, seed, exclude_na, locale, include_facts
  )

  if (df && !count_only) {
    ind <- length(ans)
    attr(ans[[ind]], "df") <- as.data.frame(ans[[ind]], locale = locale)
  }

  ans

}

# aggregation ------------------------------------------------------------------

infer_aggregation <- function(aggregate) {

  if (missing(aggregate)) {

    aggregate <- "none"

  }

  aggregate <- match.arg(
    aggregate,
    c("none", "records", "species", "taxa", "events", "documents"),
    TRUE
  )

  cond <- any(c("events", "documents") %in% aggregate) && length(aggregate) > 1L

  if (cond) {
    deferrable_error(
      "Chosen aggregation cannot by combined with other aggregations"
    )
  }

  aggregate

}

# selection --------------------------------------------------------------------

infer_selection <- function(aggregate, select, include_facts, var_type) {

  date_time_vars <- var_names[var_names[["date"]], ]

  default_vars <- switch(
    aggregate[[1L]],
    none = var_names[var_names[["default_var"]], ],
    events = var_names["gathering.gatheringId", ],
    documents = var_names["document.documentId", ],
    var_names["unit.linkings.taxon.scientificName", ]
  )

  select_type <- switch(
    aggregate[[1L]],
    none = "select",
    events = "aggregate_events",
    documents = "aggregate_documents",
    "aggregate"
  )

  if (missing(select)) {

    select <- row.names(default_vars)
    select_ <- default_vars[[var_type]]
    record_id_selected <- FALSE

    if (identical(aggregate, "none")) {
      # Missing 'select' implies default selection which implies date-time,
      # abundance, coord uncertainty and scientific name calc needed
      select <- unique(
        c(
          select, row.names(date_time_vars),
          "unit.interpretations.individualCount", "unit.abundanceString",
          "gathering.interpretations.coordinateAccuracy",
          "unit.linkings.taxon.scientificName", "unit.taxonVerbatim",
          "document.sourceId"
        )
      )
      record_id_selected <- TRUE
    }

  } else {

    deselect <- substring(grep("^-", select, value = TRUE), 2L)
    if (identical(length(deselect), length(select))) select <- "default_vars"
    select <- grep("^-", select, value = TRUE, invert = TRUE)
    select <- ifelse(
      select == "default_vars", list(default_vars[[var_type]]), select
    )
    select <- unlist(select)
    select <- select[!select %in% deselect]
    select_ <- select

    record_id_selected <- var_names["unit.unitId", var_type] %in% select

    if (!record_id_selected && identical(aggregate, "none")) {

      select <- c(var_names["unit.unitId", var_type], select)

    }

    vars <- c(
      "date_time", "eventDateTime", "date_time_ISO8601", "eventDate",
      "duration", "samplingEffort"
    )

    if (any(vars %in% select)) {

      select <- unique(c(select, date_time_vars[[var_type]]))

    }

    select <- infer_computed_vars(select, var_type)

    select_vars <- var_names[var_names[[select_type]], var_type, drop = FALSE]

    class(select_vars[[var_type]]) <- class(var_names[[var_type]])

    select <- translate(
      select, "select_vars", list(select_vars = select_vars)
    )

    vars_computed_from_id <- grepl("^computed_var_from_id", select)

    if (any(vars_computed_from_id)) {

      vars_computed_from_id <- var_names[select[vars_computed_from_id], ]

      for (i in seq_len(nrow(vars_computed_from_id))) {

        ind <- match(row.names(vars_computed_from_id)[[i]], select)
        computed_var <- vars_computed_from_id[i, var_type]
        suffix <- switch(var_type, translated_var = "_id", dwc = "ID")
        id_var <- paste0(computed_var, suffix)
        select[[ind]] <- translate(
          id_var, "select_vars", list(select_vars = select_vars)
        )

      }

    }

  }

  if (include_facts) {

    select <- c(
      select, "unit.facts.fact", "unit.facts.value", "gathering.facts.fact",
      "gathering.facts.value", "document.facts.fact", "document.facts.value"
    )

  }

  # Can't query the server for vars that are computed after download
  select <- unique(select[!grepl("^computed_var", select)])

  list(query = select, user = select_, record_id_selected = record_id_selected)

}

infer_computed_vars <- function(select, var_type) {

  abundance_vars <- c(
    "abundance", "individualCount", "occurrence_status", "occurrenceStatus"
  )

  if (any(abundance_vars %in% select)) {

    abundance_vars <- c(
      "unit.interpretations.individualCount", "unit.abundanceString"
    )

    select <- unique(c(select, var_names[abundance_vars, var_type]))

  }

  coordinates_uncertainty_vars <- c(
    "coordinates_uncertainty", "coordinateUncertaintyInMeters"
  )

  if (any(coordinates_uncertainty_vars %in% select)) {

    coordinates_uncertainty_vars <- c(
      "gathering.interpretations.coordinateAccuracy", "document.sourceId"
    )

    select <- unique(
      c(select, var_names[coordinates_uncertainty_vars, var_type])
    )

  }

  citation_vars <- c("citation", "bibliographicCitation")

  if (any(citation_vars %in% select)) {

    citation_vars <- c("document.documentId", "document.sourceId")

    select <- unique(c(select, var_names[citation_vars, var_type]))

  }

  scientific_name_vars <- c("scientific_name", "scientificName")

  if (any(scientific_name_vars %in% select)) {

    scientific_name_vars <- c(
      "unit.linkings.taxon.scientificName", "unit.taxonVerbatim",
      "unit.linkings.taxon.scientificNameAuthorship", "unit.author",
      "document.sourceId"
    )

    select <- unique(c(select, var_names[scientific_name_vars, var_type]))

  }

  red_list_vars <- c("red_list_status", "redListStatus")

  if (any(red_list_vars %in% select)) {

    red_list_vars <- c(
      "unit.linkings.taxon.latestRedListStatusFinland.status",
      "unit.linkings.taxon.latestRedListStatusFinland.year"
    )

    select <- unique(c(select, var_names[red_list_vars, var_type]))

  }

  region_vars <- c("region", "stateProvince")

  if (any(region_vars %in% select)) {

    region_vars <- "gathering.interpretations.finnishMunicipality"

    select <- unique(c(select, var_names[region_vars, var_type]))

  }

  select

}

# request ----------------------------------------------------------------------

request <- function(
  filter, select, sample, n, page, count_only, quiet, cache, query, max_size,
  select_, record_id_selected, dwc, aggregate, df, seed, exclude_na, locale,
  include_facts
) {

  path <- getOption("finbif_warehouse_query")

  if (count_only && identical(aggregate, "none")) {

    query[["selected"]] <- NULL
    query[["orderBy"]]  <- NULL
    path <- paste0(path, "unit/count")
    return(api_get(list(path = path, query = query, cache = cache)))

  }

  path <- paste0(path, select_endpoint(aggregate))

  if (count_only) {

    query[["page"]] <- 1L
    query[["pageSize"]] <- 1L
    resp <- api_get(list(path = path, query = query, cache = cache))
    resp[["content"]] <- list(total = resp[["content"]][["total"]])
    return(resp)

  }

  query[["taxonCounts"]] <- taxa_counts(aggregate)

  query[["page"]] <- page
  query[["pageSize"]] <- min(n, max_size)

  resp <- list(records_obj(path, query, cache, select, aggregate))

  n_tot <- resp[[1L]][["content"]][["total"]]
  n <- min(n, n_tot)

  if (n > max_size) {

    # If random sampling and requesting few records or a large proportion of the
    # total number of records, it makes more sense to just get all the records
    # and sample afterwards to avoid coping with duplicates due to pagination.
    sample_after_request <- n_tot < max_size * 3L || n / n_tot > .5

    if (sample && sample_after_request) {
      all_records <- finbif_records(
        filter, select_, sample = FALSE, n = n_tot, quiet = quiet,
        cache = cache, dwc = dwc, df = df, exclude_na = exclude_na,
        locale = locale, include_facts = include_facts
      )
      return(record_sample(all_records, n, cache))
    }

    resp <- get_extra_pages(
      resp, n, max_size, quiet, path, query, cache, select, aggregate, df,
      locale
    )

    if (sample) {

      if (missing(seed)) seed <- 1L

      resp <- handle_duplicates(
        resp, filter, select_, max_size, cache, n, seed, dwc, df, exclude_na,
        locale, include_facts
      )

    }

  }

  structure(
    resp, class = c("finbif_records_list", "finbif_api_list"), nrec_dnld = n,
    nrec_avl = n_tot, select = unique(select), select_user = select_,
    record_id = record_id_selected, aggregate = aggregate, cache = cache
  )

}

# construct request ------------------------------------------------------------

records_obj <- function(path, query, cache, select, aggregate) {
  structure(
    api_get(list(path = path, query = query, cache = cache)),
    class = c("finbif_records", "finbif_api"),
    select = unique(select),
    aggregate = aggregate
  )
}

# record pagination ------------------------------------------------------------

get_extra_pages <- function(
  resp, n, max_size, quiet, path, query, cache, select, aggregate, df, locale
) {

  multipage <- n > max_size

  if (multipage && !quiet) {
    pb_head("Fetching data")
    pb <- utils::txtProgressBar(0L, floor(n / max_size), style = 3L)
    on.exit(close(pb))
  }

  i <- 1L
  query[["page"]] <- query[["page"]] + 1L
  n_pages <- n %/% query[["pageSize"]]

  has_future <- has_pkgs("future")

  if (has_future) value <- future::value

  while (multipage) {

    if (!quiet) utils::setTxtProgressBar(pb, i)

    if (query[["page"]] > n_pages) {

      excess_records <- n %% query[["pageSize"]]
      last_record <- query[["pageSize"]] * n_pages
      if (last_record == n) break
      query[["pageSize"]] <- get_next_lowest_factor(last_record, excess_records)
      query[["page"]] <- last_record / query[["pageSize"]] + 1L
      n_pages <- n %/% query[["pageSize"]]

    }

    delayedAssign("res", records_obj(path, query, cache, select, aggregate))

    if (has_future) {
      res <- future::future(records_obj(path, query, cache, select, aggregate))
    }

    if (df) attr(resp[[i]], "df") <- as.data.frame(resp[[i]], locale = locale)

    i <- i + 1L

    resp[[i]] <- value(res)

    query[["page"]] <- query[["page"]] + 1L

  }

  resp

}

# parsing filters --------------------------------------------------------------

parse_filters <- function(fb_records_obj) {

  filter <- fb_records_obj[["filter"]]

  aggregate <- fb_records_obj[["aggregate"]]

  locale <- fb_records_obj[["locale"]]

  filter <- as.list(filter)
  finbif_filter_names <- translate(names(filter), "filter_names")

  cond <- switch(aggregate[[1L]], events = TRUE, documents = TRUE, FALSE)

  cond <- cond && any(c("taxonId", "target") %in% finbif_filter_names)

  if (cond) {

    deferrable_error("Cannot use current aggregation and filter by taxon")

  }

  for (i in seq_along(filter)) {

    # the filter might not exist
    if (is.na(finbif_filter_names[[i]])) next

    if (filter_names[finbif_filter_names[[i]], "translated_values"]) {
      filter[[i]] <- translate(filter[[i]], names(filter)[[i]])
    }

    if (grepl("^(not_){0,1}collection$", names(filter)[[i]])) { # nolint

      if (inherits(filter[[i]], "finbif_collections")) {

        filter[[i]] <- row.names(filter[[i]])

      } else {

        env <- list()

        env[[names(filter)[[i]]]] <- finbif_collections(
          select = NA, supercollections = TRUE, nmin = NA, locale = locale
        )

        for (cl in c("id", "collection_name", "abbreviation")) {
          class(env[[names(filter)[[i]]]][[cl]]) <- "translation"
        }

        filter[[i]] <- translate(filter[[i]], names(filter)[[i]], env)

      }

    }

    if (identical(filter_names[finbif_filter_names[[i]], "class"], "coords")) {

      # Coordinates filter must have a system defined
      check_coordinates(finbif_filter_names[[i]], filter[["coordinates"]])

      filter[[i]] <- coords(filter[[i]])

    }

    if (identical(filter_names[finbif_filter_names[[i]], "class"], "date")) {

      filter[[i]] <- dates(c(list(filter = names(filter)[[i]]), filter[[i]]))

    }

    filter[[i]] <- paste(
      filter[[i]], collapse = filter_names[finbif_filter_names[[i]], "sep"]
    )

  }

  names(filter) <- finbif_filter_names

  filter

}

check_coordinates <- function(filter_names, filter) {

  cond <- !is.null(names(filter)) && !identical(names(filter[[3]]), "")
  cond <- !utils::hasName(filter, "system") && cond
  cond <- length(filter) < 3L || cond
  cond <- identical(filter_names, "coordinates") && cond

  if (cond) deferrable_error("Invalid coordinates: system not specified")

}

# translation ------------------------------------------------------------------

translate <- function(x, translation, pos = -1) {

  trsltn <- get(translation, pos)

  # Some filters have multi-level values to translate (e.g., primary_habitat)
  if (is.list(x)) {

    names(x) <- translate(names(x), names(trsltn)[[1L]], trsltn)
    x <- lapply(x, translate, names(trsltn)[[2L]], trsltn)
    x <- lapply(x, paste, collapse = ",")
    sprintf("%s%s", names(x), ifelse(x == "", x, sprintf("[%s]", x)))

  } else {

    ind <- rep(NA_integer_, length(x))

    # multilevel filters have data.frames a level below
    if (!is.data.frame(trsltn)) trsltn <- trsltn[[1L]]

    for (i in trsltn) {
      if (inherits(i, "translation")) {
        ind_ <- match(tolower(x), tolower(i))
        ind <- ifelse(is.na(ind_), ind, ind_)
      }
    }

    if (anyNA(ind)) {
      for (err in x[is.na(ind)]) {
        deferrable_error(
          paste0("Invalid name in ", gsub("_", " ", translation), ": ", err)
        )
      }
    }

    ans <- row.names(trsltn)[ind]

    ans[!grepl("DUPLICATE", ans)]

  }

}

# sample records ---------------------------------------------------------------

record_sample <- function(x, n, cache) {

  n_tot  <- attr(x, "nrec_dnld")
  select <- attr(x, "select")
  record_id <- attr(x, "record_id")

  if (cache) {
    records <- sample_with_seed(n_tot, n_tot - n, gen_seed(x))
  } else {
    records <- sample.int(n_tot, n_tot - n)
  }

  structure(
    remove_records(x, records),
    class = c(
      "finbif_records_sample_list", "finbif_records_list", "finbif_api_list"
    ),
    nrec_dnld = n,
    nrec_avl = n_tot,
    select = select,
    record_id = record_id,
    cache = cache
  )

}

# handle duplicates ------------------------------------------------------------

handle_duplicates <- function(
  x, filter, select, max_size, cache, n, seed, dwc, df, exclude_na, locale,
  include_facts
) {

  ids <- lapply(
    x,
    function(x) {
      vapply(
        x[["content"]][["results"]], get_el_recurse, NA_character_,
        c("unit", "unitId"), "character"
      )
    }
  )

  ids <- unlist(ids)

  duplicates <- which(duplicated(ids))

  x <- remove_records(x, duplicates)

  if (length(ids) - length(duplicates) < n) {

    new_records <- finbif_records(
      filter, select, sample = TRUE, n = max_size, cache = cache, dwc = dwc,
      seed = seed, df = df, exclude_na = exclude_na, locale = locale,
      include_facts = include_facts
    )

    x[[length(x) + 1L]] <- new_records[[1L]]

    x <- handle_duplicates(
      x, filter, select, max_size, cache, n, seed + 1L, dwc, df, exclude_na,
      locale, include_facts
    )

  }

  remove_records(x, n = n)

}

# remove records ---------------------------------------------------------------

remove_records <- function(x, records, n) {

  page_sizes <- vapply(x, function(x) x[["content"]][["pageSize"]], integer(1L))
  if (missing(records)) records <- seq(1L, sum(page_sizes))[-seq(1L, n)]
  excess_pages <- rep.int(seq_along(x), page_sizes)[records]
  records <- records - c(0L, cumsum(page_sizes)[-length(x)])[excess_pages]
  records <- split(records, excess_pages)

  for (i in seq_along(x)) {
    ind <- records[[as.character(i)]]
    x[[i]][["content"]][["results"]][ind] <- NULL
    if (!is.null(ind)) {
      attr(x[[i]], "df") <- attr(x[[i]], "df")[-ind, ]
    }
    new_page_size <- length(x[[i]][["content"]][["results"]])
    x[[i]][["content"]][["pageSize"]] <- new_page_size
  }

  x[!vapply(x, function(x) x[["content"]][["pageSize"]], integer(1L))] <- NULL

  x

}

# utils ------------------------------------------------------------------------

check_n <- function(fb_records_obj) {

  n <- fb_records_obj[["n"]]
  nmax <- fb_records_obj[["nmax"]]

  if (n > nmax) {
    deferrable_error(paste("Cannot download more than", nmax, "records"))
  }

  if (n < 1L) {
    deferrable_error(paste("Cannot request less than 1 record"))
  }

}

select_endpoint <- function(aggregate) {
  switch(
    aggregate[[1L]],
    none = "unit/list",
    events = "gathering/aggregate",
    documents = "document/aggregate",
    "unit/aggregate"
  )
}

taxa_counts <- function(aggregate) {

  if (any(aggregate %in% c("species", "taxa"))) "true"

}

order_by_computed_var <- function(order_by) {

  lt <- c(
    scientific_name = "unit.linkings.taxon.scientificName",
    abundance = "unit.interpretations.individualCount",
    date_time = "gathering.eventDate.begin",
    coordinates_uncertainty = "gathering.interpretations.coordinateAccuracy"
  )

  names(lt) <- paste0("computed_var_", names(lt))

  ans <- lt[order_by]

  ans <- ifelse(is.na(ans), order_by, ans)

  as.character(ans)

}

na_exclude <- function(query, exclude_na, select_param) {

  if (exclude_na) {

    query[["hasValue"]] <- query[[select_param]]

  }

  query

}
