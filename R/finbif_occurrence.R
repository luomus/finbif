#' Download FinBIF occurrence records
#'
#' Download filtered occurrence data from FinBIF as a `data.frame`.
#'
#' @aliases fb_occurrence
#'
#' @param ... Character vectors or list of character vectors. Taxa of records
#'   to download.
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
#'   selected variables by counting records, species, taxa, individuals or
#'   events or documents. Aggregation by events or documents cannot be done in
#'   combination with any of the other aggregation types.
#' @param sample Logical. If `TRUE` randomly sample the records from the FinBIF
#'   database.
#' @param n Integer. How many records to download/import.
#' @param page Integer. Which page of records to start downloading from.
#' @param count_only Logical. Only return the number of records available.
#' @param quiet Logical. Suppress the progress indicator for multipage
#'   downloads. Defaults to value of option `finbif_hide_progress`.
#' @param cache Logical or Integer. If `TRUE` or a number greater than zero,
#'   then data-caching will be used. If not logical then cache will be
#'   invalidated after the number of hours indicated by the argument.
#' @param dwc Logical. Use Darwin Core (or Darwin Core style) variable names.
#' @param seed Integer. Set a seed for randomly sampling records.
#' @param exclude_na Logical. Should records where all selected variables have
#'   non-NA values only be returned.
#' @param locale Character. One of the supported two-letter ISO 639-1 language
#'   codes. Current supported languages are English, Finnish and Swedish. For
#'   data where more than one language is available the language denoted by
#'   `locale` will be preferred while falling back to the other languages in the
#'   order indicated above.
#' @param date_time_method Character. Passed to `lutz::tz_lookup_coords()` when
#'   `date_time` and/or `duration` variables have been selected. Default is
#'   `"fast"` when  less than 100,000 records are requested and `"none"` when
#'   more. Using method `"none"` assumes all records are in timezone
#'    "Europe/Helsinki", Use `date_time_method = "accurate"` (requires package
#'     `sf`) for greater accuracy at the cost of slower computation.
#' @param check_taxa Logical. Check first that taxa are in the FinBIF database.
#'   If true only records that match known taxa (have a valid taxon ID) are
#'   returned.
#' @param on_check_fail Character. What to do if a taxon is found not valid. One
#'   of `"warn"` (default) or `"error"`.
#' @param tzone Character. If `date_time` has been selected the timezone of the
#'   outputted date-time. Defaults to system timezone.
#' @param drop_na Logical. A vector indicating which columns to check for
#'   missing data. Values recycled to the number of columns. Defaults to all
#'   columns.
#' @param aggregate_counts Logical. Should count variables be returned when
#'   using aggregation.
#' @param unlist Logical. Should variables that contain non atomic data be
#'  concatenated into a string separated by ";"?
#' @param facts Character vector. Extra variables to be extracted from record,
#'  event and document "facts".
#' @param duplicates Logical. If `TRUE`, allow duplicate records/aggregated
#'  records when making multi-filter set requests. If `FALSE` (default)
#'  duplicate records are removed.
#' @param filter_col Character. The name of a column, with values derived from
#'  the names of the filter sets used when using multiple filters, to include
#'  when using multiple filter sets. If `NULL` (default), no column is included.
#' @param restricted_api Character. If using a restricted data API token in
#'   addition to a personal access token, a string indicating the name of an
#'   environment variable storing the restricted data API token.
#' @return A `data.frame`. If `count_only =  TRUE` an integer.
#' @examples \dontrun{
#'
#' # Get recent occurrence data for taxon
#' finbif_occurrence("Cygnus cygnus")
#'
#' # Specify the number of records
#' finbif_occurrence("Cygnus cygnus", n = 100)
#'
#' # Get multiple taxa
#' finbif_occurrence("Cygnus cygnus", "Ursus arctos")
#'
#' # Filter the records
#' finbif_occurrence(
#'   species = "Cygnus cygnus",
#'   filter = list(coordinate_accuracy_max = 100)
#' )
#'
#' }
#' @export

finbif_occurrence <- function(
  ...,
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
  date_time_method = NULL,
  check_taxa = TRUE,
  on_check_fail = c("warn", "error"),
  tzone = getOption("finbif_tz"),
  locale = getOption("finbif_locale"),
  seed = NULL,
  drop_na = FALSE,
  aggregate_counts = TRUE,
  exclude_na = FALSE,
  unlist = FALSE,
  facts = NULL,
  duplicates = FALSE,
  filter_col = NULL,
  restricted_api = NULL
) {

  fb_records_obj <- list(
    taxa = c(...),
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
    date_time_method = date_time_method,
    check_taxa = check_taxa,
    on_check_fail = match.arg(on_check_fail),
    tzone = tzone,
    locale = locale,
    df = TRUE,
    seed = seed,
    drop_na = drop_na,
    aggregate_counts = aggregate_counts,
    exclude_na = exclude_na,
    unlist = unlist,
    facts = facts,
    duplicates = duplicates,
    filter_col = filter_col,
    restricted_api = restricted_api
  )

  occurrence(fb_records_obj)

}

#' @noRd

occurrence <- function(fb_records_obj) {

  fb_records_obj <- select_taxa(fb_records_obj)

  fb_records_obj <- det_datetime_method(fb_records_obj)

  if (use_multi_req(fb_records_obj)) {

    return(multi_req(fb_records_obj))

  }

  fb_records_obj[["filter"]] <- c(
    fb_records_obj[["taxa"]], fb_records_obj[["filter"]]
  )

  facts <- fb_records_obj[["facts"]]

  fb_records_obj[["include_facts"]] <- !is.null(facts)

  n <- fb_records_obj[["n"]]

  needs_n <- n < 0 || is.factor(n) || !is.finite(n)

  if (needs_n) {

    max_page_size <- getOption("finbif_max_page_size")

    fb_records_obj[["n"]] <- max_page_size

    records <- records(fb_records_obj)

    n <- attr(records, "nrec_avl")

    fb_records_obj[["n"]] <- pmax(n, max_page_size)

  }

  records <- records(fb_records_obj)

  if (fb_records_obj[["count_only"]]) {

    return(records[[c("content", "total")]])

  }

  quiet <- fb_records_obj[["quiet"]] || length(records) < 2L

  attr(records, "quiet") <- pb_head("Processing data", quiet = quiet)

  fb_occurrence_df <- records_list_data_frame(records)

  colnames <- names(fb_occurrence_df)

  n_col_nms <- grep("^n_", colnames, value = TRUE)

  ind <- !colnames %in% n_col_nms

  dwc <- fb_records_obj[["dwc"]]

  non_count_cols <- colnames[ind]

  var_names <- sysdata("var_names")

  colnames[ind] <- var_names[non_count_cols, col_type_string(dwc)]

  select_user <- attr(records, "select_user", TRUE)

  if (fb_records_obj[["aggregate_counts"]]) {

    select_user <- c(select_user, n_col_nms)

  }

  select_user <- name_chr_vec(select_user)

  df_attrs <- attributes(fb_occurrence_df)

  new_attrs <- list(
    nrec_dnld = attr(records, "nrec_dnld", TRUE),
    nrec_avl  = attr(records, "nrec_avl", TRUE),
    column_names = select_user,
    aggregate = fb_records_obj[["aggregate"]],
    dwc = dwc,
    date_time = attr(records, "date_time", TRUE),
    date_time_method = fb_records_obj[["date_time_method"]],
    tzone = fb_records_obj[["tzone"]],
    locale = fb_records_obj[["locale"]],
    include_new_cols = TRUE,
    facts = facts,
    unlist = fb_records_obj[["unlist"]],
    drop_na = fb_records_obj[["drop_na"]]
  )

  df_attrs <- c(df_attrs, new_attrs)

  df_attrs[["names"]] <- colnames

  attributes(fb_occurrence_df) <- df_attrs

  fb_occurrence_df <- date_times(fb_occurrence_df)

  fb_occurrence_df <- compute_date_time(fb_occurrence_df)

  fb_occurrence_df <- compute_duration(fb_occurrence_df)

  fb_occurrence_df <- compute_iso8601(fb_occurrence_df)

  fb_occurrence_df <- compute_vars_from_id(fb_occurrence_df)

  fb_occurrence_df <- compute_epsg(fb_occurrence_df)

  fb_occurrence_df <- compute_abundance(fb_occurrence_df)

  fb_occurrence_df <- compute_citation(fb_occurrence_df)

  fb_occurrence_df <- compute_coordinate_uncertainty(fb_occurrence_df)

  fb_occurrence_df <- compute_scientific_name(fb_occurrence_df)

  fb_occurrence_df <- compute_red_list_status(fb_occurrence_df)

  fb_occurrence_df <- compute_region(fb_occurrence_df)

  fb_occurrence_df <- compute_codes(fb_occurrence_df)

  fb_occurrence_df <- extract_facts(fb_occurrence_df)

  class(fb_occurrence_df) <- c("finbif_occ", "data.frame")

  facts <- name_chr_vec(facts)

  select_user <- c(select_user, facts)

  fb_occurrence_df <- fb_occurrence_df[select_user]

  attr(fb_occurrence_df, "column_names") <- select_user

  fb_occurrence_df <- unlist_cols(fb_occurrence_df)

  names(fb_occurrence_df) <- names(select_user)

  drop_na_col(fb_occurrence_df)

}

#' @noRd

use_multi_req <- function(fb_records_obj) {

  filter <- fb_records_obj[["filter"]]

  nms <- names(filter)

  fnames <- sysdata("filter_names")

  no_name <- is.null(nms) || !any(nms %in% fnames[["translated_filter"]])

  filters_has_no_length <- vapply(filter, length, 0L) < 1L

  filter_names <- lapply(filter, names)

  filter_names_are_char <- vapply(filter_names, is.character, NA)

  named <- all(filters_has_no_length | filter_names_are_char)

  no_name && named && is.list(filter) && length(filter) > 0L

}

#' @noRd
#' @importFrom utils txtProgressBar setTxtProgressBar

records_list_data_frame <- function(x) {

  verbose <- !attr(x, "quiet")

  n <- length(x)

  if (verbose) {

    pb <- utils::txtProgressBar(0L, n, style = 3L)

    on.exit(close(pb))

  }

  df <- list()

  for (i in seq_len(n)) {

    if (verbose) {

      utils::setTxtProgressBar(pb, i)

    }

    xi <- x[[i]]

    df[[i]] <- attr(xi, "df")

  }

  url <- vapply(df, attr, "", "url", TRUE)

  time <- lapply(df, attr, "time", TRUE)

  df <- do.call(rbind, df)

  record_id <- switch(
    xi[["aggregate"]], none = "unit.unitId", xi[["select_query"]]
  )

  record_id <- do.call(paste, df[, record_id, drop = FALSE])

  dups <- duplicated(record_id)

  df <- df[!dups, , drop = FALSE]

  record_id <- record_id[!dups]

  s <- seq_len(attr(x, "nrec_dnld", TRUE))

  if (attr(x, "sample", TRUE)) {

    s <- sample(s)

    if (attr(x, "cache", TRUE)) {

      seed <- gen_seed(x)

      s <- sample_with_seed(attr(x, "nrec_dnld", TRUE), seed)

    }

  }

  df <- df[s, , drop = FALSE]

  record_id <- record_id[s]

  if (!attr(x, "record_id")) {

    df[["unit.unitId"]] <- NULL

  }

  structure(df, url = url, time = do.call(c, time), record_id = record_id)

}

#' @noRd

det_datetime_method <- function(fb_records_obj) {

  method <- fb_records_obj[["date_time_method"]]

  if (is.null(method)) {

    method <- "none"

    n <- fb_records_obj[["n"]]

    is_num <- is.numeric(n)

    n <- ifelse(is_num & n >= 0L, n, Inf)

    n <- sum(n)

    if (n < 1e5L) {

      method <- "fast"

    }

    fb_records_obj[["date_time_method"]] <- method

  }

  fb_records_obj

}

#' @noRd

select_taxa <- function(fb_records_obj) {

  taxa <- fb_records_obj[["taxa"]]

  ntaxa <- length(taxa)

  if (ntaxa > 0L) {

    taxon_name <- paste(taxa, collapse = ",")

    ans <- list(taxon_name = taxon_name)

    if (fb_records_obj[["check_taxa"]]) {

      cache <- fb_records_obj[["cache"]]

      if (!"taxa" %in% names(taxa) || ntaxa > 1L) {

        taxa <- finbif_check_taxa(taxa, cache = cache)

      } else {

        cache <- list(cache = cache)

        taxa <- as.list(taxa)

        taxa <- c(taxa, cache)

        taxa <- do.call(finbif_check_taxa, taxa)

      }

      taxa <- unlist(taxa)

      taxa_invalid <- is.na(taxa)

      if (any(taxa_invalid)) {

        invalid_taxa_names <- names(taxa[taxa_invalid])

        msg <- sub("\\.", " - ", invalid_taxa_names)

        msg <- paste(msg, collapse = ", ")

        msg <- paste(
          "Cannot find the following taxa in the FinBIF taxonomy.",
          "Please check you are using accepted names and not synonyms or",
          "other names for the taxa you are selecting:\n",
          msg,
          sep = "\n"
        )

        switch(
          fb_records_obj[["on_check_fail"]],
          warn  = warning(msg, call. = FALSE),
          error = stop(msg, call. = FALSE)
        )

      }

      taxa_valid <- !taxa_invalid

      if (any(taxa_valid)) {

        valid_taxa <- paste(taxa[taxa_valid], collapse = ",")

        ans <- list(taxon_id = valid_taxa)

      }

    }

    fb_records_obj[["taxa"]] <- ans

  }

  fb_records_obj

}

#' @noRd

date_times <- function(fb_occurrence_df) {

  if (attr(fb_occurrence_df, "date_time", TRUE)) {

    dwc <- attr(fb_occurrence_df, "dwc", TRUE)

    vtype <- col_type_string(dwc)

    var_names <- sysdata("var_names")

    date_start <- var_names["gathering.eventDate.begin", vtype]

    hour_start <- var_names["gathering.hourBegin", vtype]

    minute_start <- var_names["gathering.minutesBegin", vtype]

    date_end <- var_names["gathering.eventDate.end", vtype]

    hour_end <- var_names["gathering.hourEnd", vtype]

    minute_end <- var_names["gathering.minutesEnd", vtype]

    lat <- var_names["gathering.conversions.wgs84CenterPoint.lat", vtype]

    lon <- var_names["gathering.conversions.wgs84CenterPoint.lon", vtype]

    tzone <- attr(fb_occurrence_df, "tzone", TRUE)

    method <- attr(fb_occurrence_df, "date_time_method", TRUE)

    lat <- fb_occurrence_df[[lat]]

    lon <- fb_occurrence_df[[lon]]

    date_time_start <- list(
      date = fb_occurrence_df[[date_start]],
      hour = fb_occurrence_df[[hour_start]],
      minute = fb_occurrence_df[[minute_start]],
      lat = lat,
      lon = lon,
      tzone = tzone,
      method = method
    )

    date_time_end <- list(
      date = fb_occurrence_df[[date_end]],
      hour = fb_occurrence_df[[hour_end]],
      minute = fb_occurrence_df[[minute_end]],
      lat = lat,
      lon = lon,
      tzone = tzone,
      method = method
    )

    df_attrs <- attributes(fb_occurrence_df)

    new_attrs <- list(
      date_time_start = date_time(date_time_start),
      date_time_end = date_time(date_time_end)
    )

    attributes(fb_occurrence_df) <-  c(df_attrs, new_attrs)

  }

  fb_occurrence_df

}

#' @noRd
#' @importFrom lutz tz_lookup_coords

date_time <- function(date_time_obj) {

  tzone <- date_time_obj[["tzone"]]

  date_time <- character()

  date_time <- as.POSIXct(date_time, tz = tzone)

  date <- date_time_obj[["date"]]

  if (length(date) > 0L) {

    date_time <- as.Date(date)

    date_time <- as.POSIXlt(date_time)

    hour <- date_time_obj[["hour"]]

    if (!is.null(hour)) {

      hour_is_na <- is.na(hour)

      date_time[, "hour"] <- ifelse(hour_is_na, 12L, hour)

    }

    minute <- date_time_obj[["minute"]]

    if (!is.null(minute)) {

      date_time_min <- date_time[, "min"]

      min_is_na <- is.na(minute)

      date_time[, "min"] <- ifelse(min_is_na, date_time_min, minute)

    }

    date_time <- format(date_time, "%F %T")

    method <- date_time_obj[["method"]]

    if (identical(method, "none")) {

      date_time <- as.POSIXct(date_time, "Europe/Helsinki")

    } else {

      tz_in <- lutz::tz_lookup_coords(
        date_time_obj[["lat"]],
        date_time_obj[["lon"]],
        method,
        FALSE
      )

      tz_in_is_na <- is.na(tz_in)

      tz_in <- ifelse(tz_in_is_na, "Europe/Helsinki", tz_in)

      date_time <- mapply(
        as.POSIXct, date_time, tz_in, SIMPLIFY = FALSE, USE.NAMES = FALSE
      )

      date_time <-  do.call(c, date_time)

    }

    date_time <- as.POSIXct(date_time, tzone)

    na_m <- is.na(date_time_obj[["month"]])

    na_d <- is.na(date_time_obj[["day"]])

    date_time[na_m | na_d] <- as.POSIXct(NA_integer_, tz = tzone)

  }

  date_time

}

#' @noRd

compute_date_time <- function(fb_occurrence_df) {

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  var_names <- sysdata("var_names")

  dtv <- var_names["computed_var_date_time",  col_type_string(dwc)]

  if (dtv %in% attr(fb_occurrence_df, "column_names", TRUE)) {

    fb_occurrence_df[[dtv]] <- attr(fb_occurrence_df, "date_time_start", TRUE)

  }

  fb_occurrence_df

}

#' @noRd

compute_duration <- function(fb_occurrence_df) {

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

  var_names <- sysdata("var_names")

  duration_var <- var_names["computed_var_duration", vtype]

  if (duration_var %in% attr(fb_occurrence_df, "column_names", TRUE)) {

    h_start <- var_names["gathering.hourBegin", vtype]

    h_end <- var_names["gathering.hourEnd", vtype]

    hna <- is.na(fb_occurrence_df[[h_start]]) | is.na(fb_occurrence_df[[h_end]])

    datetime_start <- attr(fb_occurrence_df, "date_time_start", TRUE)

    datetime_end <- attr(fb_occurrence_df, "date_time_end", TRUE)

    dna <- is.na(datetime_start) | is.na(datetime_end)

    da <- !hna & !dna

    duration_length <- length(da)

    intvl <- rep_len(NA_integer_, duration_length)

    intvl <- as.difftime(intvl, units = "secs")

    fb_occurrence_df[[duration_var]] <- intvl

    intvl[da] <- difftime(datetime_end[da], datetime_start[da])

    has_duration <- da & intvl != 0

    fb_occurrence_df[has_duration, duration_var] <- intvl[has_duration]

  }

  fb_occurrence_df

}

#' @noRd

compute_iso8601 <- function(fb_occurrence_df) {

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

  var_names <- sysdata("var_names")

  iso8601_var <- var_names["computed_var_date_time_ISO8601", vtype]

  if (iso8601_var %in% attr(fb_occurrence_df, "column_names", TRUE)) {

    ds <- attr(fb_occurrence_df, "date_time_start", TRUE)

    dsna <- is.na(ds)

    de <- attr(fb_occurrence_df, "date_time_end", TRUE)

    dena <- is.na(de)

    duration_na <- dsna | dena

    duration_length <- length(duration_na)

    iso8601s <- rep_len(NA_integer_, duration_length)

    tzone <- attr(fb_occurrence_df, "tzone", TRUE)

    iso8601s <- as.POSIXct(iso8601s, tzone)

    iso8601e <- iso8601s

    iso8601s[!duration_na] <- ds[!dsna]

    iso8601e[!duration_na] <- de[!dena]

    iso8601s <- format(iso8601s, "%FT%T%z")

    iso8601e <- format(iso8601e, "%FT%T%z")

    iso8601 <- paste(iso8601s, iso8601e, sep = "/")

    hour_start <- var_names["gathering.hourBegin", vtype]

    hour_start <- fb_occurrence_df[[hour_start]]

    minute_start <- var_names["gathering.minutesBegin", vtype]

    minute_start <- fb_occurrence_df[[minute_start]]

    hour_end <- var_names["gathering.hourEnd", vtype]

    hour_end <- fb_occurrence_df[[hour_end]]

    minute_end <- var_names["gathering.minutesEnd", vtype]

    minute_end <- fb_occurrence_df[[minute_end]]

    no_start_time <- is.na(hour_start) & is.na(minute_start)

    no_end_time <- is.na(hour_end) & is.na(minute_end)

    date_start <- var_names["gathering.eventDate.begin", vtype]

    date_start <- fb_occurrence_df[[date_start]]

    date_end <- var_names["gathering.eventDate.end", vtype]

    date_end <- fb_occurrence_df[[date_end]]

    ds_time <- as.POSIXct(date_start)

    de_time <- as.POSIXct(date_end)

    ds_time <- format(ds_time, "%FT%T%z")

    de_time  <- format(de_time, "%FT%T%z")

    format_interval <- iso8601 <- paste(ds_time, de_time, sep = "/")

    iso8601 <- ifelse(no_start_time | no_end_time, format_interval, iso8601)

    dates_equal <- date_start == date_end

    no_end_date <-  is.na(date_end) | dates_equal

    no_end <- no_end_time & no_end_date

    use_start <- no_end | ds == de

    format_start <- format(ds, "%FT%T%z")

    iso8601 <- ifelse(use_start, format_start, iso8601)

    fmt_start_ymd <- as.Date(date_start)

    fmt_start_ymd <- format(fmt_start_ymd, "%FT%T%z")

    iso8601 <- ifelse(no_start_time & no_end_date, fmt_start_ymd, iso8601)

    iso8601_na <- is.na(iso8601)

    date_intvl <- paste(date_start, date_end, sep = "/")

    date_intvl <- ifelse(dates_equal, date_start, date_intvl)

    fb_occurrence_df[[iso8601_var]] <- ifelse(iso8601_na, date_intvl, iso8601)

  }

  fb_occurrence_df

}

#' @noRd

compute_vars_from_id <- function(fb_occurrence_df) {

  locale <- attr(fb_occurrence_df, "locale")

  add <- attr(fb_occurrence_df, "include_new_cols")

  colnames <- names(fb_occurrence_df)

  select_user <- attr(fb_occurrence_df, "column_names")

  cols <- setdiff(select_user, colnames)

  dwc <- attr(fb_occurrence_df, "dwc")

  vtype <- col_type_string(dwc)

  suffix <- switch(vtype, translated_var = "_id", dwc = "ID")

  for (i in seq_along(cols)) {

    col_i <- cols[[i]]

    id_var_name <- paste0(col_i, suffix)

    if (add && id_var_name %in% colnames) {

      if (identical(id_var_name, "collection_id")) {

        ptrn <- "collection_name"

        metadata <- finbif_collections(
          select = ptrn,
          subcollections = TRUE,
          supercollections = TRUE,
          nmin = NA,
          locale = locale
        )

      } else {

        ptrn <- "^name_|^description_"

        col_i_native <- to_native(col_i)

        metadata <- sysdata(col_i_native)

        if (!inherits(metadata, "data.frame")) {

          rownames <- lapply(metadata, row.names)

          args <- c(metadata, make.row.names = FALSE)

          metadata <- do.call(rbind, args)

          row.names(metadata) <- unlist(rownames)

        }

      }

      id_var <- fb_occurrence_df[[id_var_name]]

      nms <- names(metadata)

      names(metadata) <- gsub(ptrn, "", nms)

      id <- lapply(id_var, vapply, remove_domain, "")

      metadata <- metadata[, grep(ptrn, nms), drop = FALSE]

      var <- lapply(id, get_rows, metadata)

      var <- lapply(var, apply, 1L, with_locale, locale)

      var <- lapply(var, unname)

      var_is_na <- lapply(var, is.na)

      df_col_i <- mapply(
        ifelse, var_is_na, id_var, var, SIMPLIFY = FALSE, USE.NAMES = FALSE
      )

      if (!is.list(id_var)) {

        df_col_i <- unlist(df_col_i)

      }

      if (is.null(df_col_i)) {

        df_col_i <- character()

      }

      fb_occurrence_df[[col_i]] <- df_col_i

    }

  }

  fb_occurrence_df

}

#' @noRd

compute_epsg <- function(fb_occurrence_df) {

  if (attr(fb_occurrence_df, "include_new_cols", TRUE)) {

    dwc <- attr(fb_occurrence_df, "dwc", TRUE)

    vtype <- col_type_string(dwc)

    var_names <- sysdata("var_names")

    vnt <- var_names[, vtype, drop = FALSE]

    epsg_vars <- c(vnt["computed_var_epsg", ], vnt["computed_var_fp_epsg", ])

    select_user <- attr(fb_occurrence_df, "column_names", TRUE)

    crs <- c(computed_var_epsg = "", computed_var_fp_epsg = "footprint")

    crs_nms <- names(crs)

    sq <- seq_along(crs)

    sq <- sq[epsg_vars %in% select_user]

    select_user <- match(select_user, vnt[[1L]])

    select_user <- var_names[select_user, ]

    select_user <- row.names(select_user)

    select_user <- var_names[select_user, "translated_var"]

    epsgs <- c(euref = "euref", ykj = "ykj", wgs84 = "wgs84")

    for (i in sq) {

      epsg <- epsgs

      epsg[] <- paste0(crs[[i]], "_", epsgs, "$")

      epsg <- lapply(epsg, grepl, select_user)

      epsg <- lapply(epsg, c, TRUE)

      epsg <- lapply(epsg, which)

      epsg <- vapply(epsg, min, 0L, USE.NAMES = TRUE)

      epsg <- which.min(epsg)

      crs_nm_i <- crs_nms[[i]]

      crs_nm_i <- var_names[[crs_nm_i, vtype]]

      fb_occurrence_df[[crs_nm_i]] <- switch(
        names(epsg),
        euref = "EPSG:3067",
        ykj = "EPSG:2393",
        wgs84 = "EPSG:4326",
        NA_character_
      )

    }

  }

  fb_occurrence_df

}

#' @noRd

compute_abundance <- function(fb_occurrence_df) {

  select_user <- attr(fb_occurrence_df, "column_names", TRUE)

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

  vnms <- sysdata("var_names")

  abundance_var <- vnms[["computed_var_abundance", vtype]]

  has_abundance <- abundance_var %in% select_user

  occ_status_var <- vnms[["computed_var_occurrence_status", vtype]]

  has_occ <- occ_status_var %in% select_user

  has_abundance_vars <- has_abundance || has_occ

  if (attr(fb_occurrence_df, "include_new_cols", TRUE) && has_abundance_vars) {

    count_var <- vnms[["unit.interpretations.individualCount", vtype]]

    count <- fb_occurrence_df[[count_var]]

    verbatim_var <- vnms[["unit.abundanceString", vtype]]

    has_one <- grepl("1", fb_occurrence_df[[verbatim_var]])

    has_one <- ifelse(has_one, 1L, NA_integer_)

    abundance <- ifelse(count == 1L, has_one, count)

    if (has_abundance) {

      fb_occurrence_df[[abundance_var]] <- abundance

    }

    if (has_occ) {

      stat <- switch(
        attr(fb_occurrence_df, "locale", TRUE),
        fi = c("paikalla", "poissa"),
        sv = c("n\u00e4rvarande", "fr\u00e5nvarande"),
        c("present", "absent")
      )

      is_p <- is.na(abundance) | abundance > 0L

      fb_occurrence_df[[occ_status_var]] <- ifelse(is_p, stat[[1L]], stat[[2L]])

    }

  }

  fb_occurrence_df

}

#' @noRd

compute_citation <- function(fb_occurrence_df) {

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

  var_names <- sysdata("var_names")

  citation_var <- var_names[["computed_var_citation", vtype]]

  has_cit_var <- citation_var %in% attr(fb_occurrence_df, "column_names", TRUE)

  if (attr(fb_occurrence_df, "include_new_cols", TRUE) && has_cit_var) {

    r_id <- attr(fb_occurrence_df, "record_id", TRUE)

    src <- var_names[["document.sourceId", vtype]]

    document_id_var <- var_names[["document.documentId", vtype]]

    d_id <- fb_occurrence_df[[document_id_var]]

    cit <- ifelse(fb_occurrence_df[[src]] == "http://tun.fi/KE.3", d_id, r_id)

    fb_occurrence_df[[citation_var]] <- paste(cit, "Source: FinBIF")

  }

  fb_occurrence_df

}

#' @noRd

compute_coordinate_uncertainty <- function(fb_occurrence_df) {

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

  vnms <- sysdata("var_names")

  uncert_var <- vnms[["computed_var_coordinates_uncertainty", vtype]]

  add <- attr(fb_occurrence_df, "include_new_cols", TRUE)

  if (add && uncert_var %in% attr(fb_occurrence_df, "column_names", TRUE)) {

    interp_var <- vnms[["gathering.interpretations.coordinateAccuracy", vtype]]

    interp <- fb_occurrence_df[[interp_var]]

    source_var <- vnms[["document.sourceId", vtype]]

    na <- fb_occurrence_df[[source_var]] == "http://tun.fi/KE.3" &  interp == 1

    coord_uncert <- ifelse(na, NA_real_, interp)

    fb_occurrence_df[[uncert_var]] <- as.numeric(coord_uncert)

  }

  fb_occurrence_df

}

#' @noRd

compute_scientific_name <- function(fb_occurrence_df) {

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

  var_names <- sysdata("var_names")

  sci_var <- var_names[["computed_var_scientific_name", vtype]]

  add <- attr(fb_occurrence_df, "include_new_cols", TRUE)

  if (add && sci_var %in% attr(fb_occurrence_df, "column_names", TRUE)) {

    sci_interp <- var_names[["unit.linkings.taxon.scientificName", vtype]]

    sci_interps <- fb_occurrence_df[[sci_interp]]

    verbatim <- var_names[["unit.taxonVerbatim", vtype]]

    verbatims <- fb_occurrence_df[[verbatim]]

    author <- var_names[["unit.linkings.taxon.scientificNameAuthorship", vtype]]

    authors <- fb_occurrence_df[[author]]

    verbatim_author <- var_names[["unit.author", vtype]]

    verbatim_authors <- fb_occurrence_df[[verbatim_author]]

    with_verbatim <- list(names = verbatims, authors = verbatim_authors)

    with_verbatim <- add_authors(with_verbatim)

    without_verbatim <- list(names = sci_interps, authors = authors)

    without_verbatim <- add_authors(without_verbatim)

    src <- var_names[["document.sourceId", vtype]]

    uv <- fb_occurrence_df[[src]] == "http://tun.fi/KE.3" & is.na(sci_interps)

    fb_occurrence_df[[sci_var]] <- ifelse(uv, with_verbatim, without_verbatim)

  }

  fb_occurrence_df

}

#' @noRd

compute_codes <- function(fb_occurrence_df) {

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  col_names <- attr(fb_occurrence_df, "column_names", TRUE)

  add <- attr(fb_occurrence_df, "include_new_cols", TRUE)

  vtype <- col_type_string(dwc)

  var_names <- sysdata("var_names")

  id_var <- var_names[["document.collectionId", vtype]]

  codevars <- c("computed_var_collection_code", "computed_var_institution_code")

  for (i in seq_along(codevars)) {

    codevar_i <- codevars[[i]]

    var <- var_names[[codevar_i, vtype]]

    if (add && var %in% col_names) {

      codes <- finbif_collections(
        select = c("collection_code", "institution_code"),
        supercollections = TRUE,
        nmin = NA
      )

      id <- fb_occurrence_df[[id_var]]

      id <- remove_domain(id)

      fb_occurrence_df[[var]] <- codes[id, i]

    }

  }

  fb_occurrence_df

}

#' @noRd

add_authors <- function(names_obj) {

  names <- names_obj[["names"]]

  authors <- names_obj[["authors"]]

  has_authors <- !is.na(authors) & nchar(authors) > 0L

  authors <- paste0(" ", authors)

  authors <- ifelse(has_authors, authors, "")

  with_authors <- paste0(names, authors)

  names_na <- is.na(names)

  ifelse(names_na, names, with_authors)

}

#' @noRd

compute_red_list_status <- function(fb_occurrence_df) {

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

  vn <- sysdata("var_names")

  red_list_var <- vn[["computed_var_red_list_status", vtype]]

  add <- attr(fb_occurrence_df, "include_new_cols", TRUE)

  if (add && red_list_var %in% attr(fb_occurrence_df, "column_names", TRUE)) {

    id <- vn[["unit.linkings.taxon.latestRedListStatusFinland.status", vtype]]

    id <- fb_occurrence_df[[id]]

    yr <- vn[["unit.linkings.taxon.latestRedListStatusFinland.year", vtype]]

    na <- is.na(id)

    red_list <- sub("http://tun.fi/MX.iucn", "", id)

    red_list <- paste(red_list, fb_occurrence_df[[yr]])

    fb_occurrence_df[[red_list_var]] <- ifelse(na, NA_character_, red_list)

  }

  fb_occurrence_df

}

#' @noRd

compute_region <- function(fb_occurrence_df) {

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

  var_names <- sysdata("var_names")

  region_var <- var_names[["computed_var_region", vtype]]

  add <- attr(fb_occurrence_df, "include_new_cols", TRUE)

  if (add && region_var %in% attr(fb_occurrence_df, "column_names", TRUE)) {

    idv <- var_names[["gathering.interpretations.finnishMunicipality", vtype]]

    id <- basename(fb_occurrence_df[[idv]])

    municipality <- municipality()

    fb_occurrence_df[[region_var]] <- municipality[id, "region"]

  }

  fb_occurrence_df

}

#' @noRd

multi_req <- function(fb_records_obj) {

  filters <- fb_records_obj[["filter"]]

  filter_num <- seq_along(filters)

  filter_nms <- names(filters)

  if (is.null(filter_nms)) {

    filter_nms <- filter_num

  } else {

    filter_nms <- ifelse(filter_nms == "", filter_num, filter_nms)

  }

  n_filters <- length(filters)

  ans <- vector("list", n_filters)

  rep_args <- c(
    "sample",
    "n",
    "page",
    "quiet",
    "cache",
    "date_time_method",
    "tzone",
    "locale",
    "exclude_na"
  )

  for (arg in rep_args) {

    fb_records_obj[[arg]] <- rep_len(fb_records_obj[[arg]], n_filters)

  }

  fb_records_obj_filter <- fb_records_obj

  fb_records_obj_filter[["check_taxa"]] <- FALSE

  for (filter in seq_len(n_filters)) {

    for (arg in c("filter", rep_args)) {

      fb_records_obj_arg <- fb_records_obj[[arg]]

      fb_records_obj_filter[[arg]] <- fb_records_obj_arg[[filter]]

    }

    ans[[filter]] <- occurrence(fb_records_obj_filter)

  }

  if (!fb_records_obj[["count_only"]]) {

    filter_col <- fb_records_obj[["filter_col"]]

    if (!is.null(filter_col)) {

      for (i in filter_num) {

        ans_i <- ans[[i]]

        filter_nms_i <- filter_nms[[i]]

        if (nrow(ans_i) < 1L) {

          filter_nms_i <- character()

        }

        ans_i[[filter_col]]

        ans[[i]] <- ans_i

      }

    }

    ans <- do.call(rbind, ans)

    if (!fb_records_obj[["duplicates"]]) {

      record_id <- attr(ans, "record_id")

      ans <- ans[!duplicated(record_id), ]

    }

  }

  ans

}

#' @noRd

unlist_cols <- function(fb_occurrence_df) {

  if (attr(fb_occurrence_df, "unlist", TRUE)) {

    for (col in attr(fb_occurrence_df, "column_names", TRUE)) {

      df_col <- fb_occurrence_df[[col]]

      if (is.list(df_col) && !grepl("Fact|fact_", col)) {

        fb_occurrence_df[[col]] <- vapply(df_col, concat_string, "")

      }

    }

  }

  fb_occurrence_df

}

#' @noRd

drop_na_col <- function(fb_occurrence_df) {

  drop_which <- attr(fb_occurrence_df, "drop_na", TRUE)

  if (any(drop_which)) {

    fb_occurrence_df_attrs <- attributes(fb_occurrence_df)

    fb_occurrence_df_attrs[["class"]] <- class(fb_occurrence_df)

    nrows <- nrow(fb_occurrence_df)

    fb_occurrence_df_attrs[["row.names"]] <- seq_len(nrows)

    attr(fb_occurrence_df, "class") <- "list"

    is_na <- lapply(fb_occurrence_df, is.na)

    ncols <- length(fb_occurrence_df)

    drop <- vapply(is_na, all, NA) & rep_len(drop_which, ncols)

    column_names <- attr(fb_occurrence_df, "column_names", TRUE)

    drop_column_names <- column_names[drop]

    column_names <- column_names[!drop]

    fb_occurrence_df_attrs[["column_names"]] <- column_names

    fb_occurrence_df_attrs[["names"]] <- column_names

    for (i in drop_column_names) {

      fb_occurrence_df[[i]] <- NULL

    }

    attributes(fb_occurrence_df) <- fb_occurrence_df_attrs

  }

  fb_occurrence_df

}

#' @noRd

extract_facts <- function(fb_occurrence_df) {

  facts <- attr(fb_occurrence_df, "facts", TRUE)

  if (!is.null(facts)) {

    dwc <- attr(fb_occurrence_df, "dwc", TRUE)

    vtype <- col_type_string(dwc)

    nms <- c(
      "unit.facts.fact",
      "gathering.facts.fact",
      "document.facts.fact"
    )

    vls <- c(
      "unit.facts.value",
      "gathering.facts.value",
      "document.facts.value"
    )

    fb_occurrence_df[facts] <- NA_character_

    var_names <- sysdata("var_names")

    for (fact in facts) {

      for (level in 1:3) {

        levels_nms <- nms[[level]]

        fact_name <- var_names[[levels_nms, vtype]]

        facts_sans_domain <- lapply(
          fb_occurrence_df[[fact_name]], remove_domain
        )

        fact_sans_domain <- remove_domain(fact)

        is_fact <- lapply(facts_sans_domain, "==", fact_sans_domain)

        level_vls <- vls[[level]]

        values_name <- var_names[[level_vls, vtype]]

        fb_occurrence_df[[fact]] <- mapply(
          extract_fact, fb_occurrence_df[[values_name]], is_fact
        )

      }

    }

  }

  fb_occurrence_df

}

#' @noRd

extract_fact <- function(
  x,
  i
) {

  ans <- x[i]

  if (length(ans) < 1L) {

    ans <- NA_character_

  }

  ans


}

#' Get last modified date for FinBIF occurrence records
#'
#' Get last modified date for filtered occurrence data from FinBIF.
#'
#' @aliases fb_last_mod
#'
#' @inheritParams finbif_occurrence
#' @return A `Date` object
#' @examples \dontrun{
#'
#' # Get last modified date for Whooper Swan occurrence records from Finland
#' finbif_last_mod("Cygnus cygnus", filter = c(country = "Finland"))
#'
#' }
#' @export

finbif_last_mod <- function(
  ...,
  filter
) {

  if (missing(filter)) {

    filter <- NULL

  }

  res <- finbif_occurrence(
    ..., filter = filter, select = "load_date", order_by = "-load_date", n = 1L
  )

  ans <- as.Date(character())

  if (nrow(res) > 0L) {

    ans <- as.Date(res[[1L, "load_date"]])

  }

  ans

}
