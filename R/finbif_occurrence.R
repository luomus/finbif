#' Download FinBIF occurrence records
#'
#' Download filtered occurrence data from FinBIF as a `data.frame`.
#'
#' @aliases fb_occurrence
#'
#' @param ... Character vectors or list of character vectors. Taxa of records
#'   to download.
#' @inheritParams finbif_records
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
#' @importFrom lubridate as_datetime as.duration as.interval force_tzs
#' @importFrom lubridate format_ISO8601 hour interval minute ymd
#' @importFrom lutz tz_lookup_coords
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
  facts = NULL
) {

  taxa <- c(...)

  on_check_fail <- match.arg(on_check_fail)

  fb_records_obj <- list(
    taxa = taxa,
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
    on_check_fail = on_check_fail,
    tzone = tzone,
    locale = locale,
    df = TRUE,
    seed = seed,
    drop_na = drop_na,
    aggregate_counts = aggregate_counts,
    exclude_na = exclude_na,
    unlist = unlist,
    facts = facts
  )

  fb_records_obj <- select_taxa(fb_records_obj)

  fb_records_obj <- det_datetime_method(fb_records_obj)

  occurrence(fb_records_obj)

}

#' @noRd

occurrence <- function(fb_records_obj) {

  filter <- fb_records_obj[["filter"]]

  taxa <- fb_records_obj[["taxa"]]

  n <- fb_records_obj[["n"]]

  date_time_method <- fb_records_obj[["date_time_method"]]

  aggregate <- fb_records_obj[["aggregate"]]

  count_only <- fb_records_obj[["count_only"]]

  quiet <- fb_records_obj[["quiet"]]

  dwc <- fb_records_obj[["dwc"]]

  locale <- fb_records_obj[["locale"]]

  facts <- fb_records_obj[["facts"]]

  aggregate_counts <- fb_records_obj[["aggregate_counts"]]

  tzone <- fb_records_obj[["tzone"]]

  unlist <- fb_records_obj[["unlist"]]

  drop_na <- fb_records_obj[["drop_na"]]

  filter_names <- names(filter)

  no_filter_names <- is.null(filter_names)

  multi_filter <- no_filter_names && !is.null(filter)

  if (multi_filter) {

    aggregate_none <- identical(aggregate, "none")

    stopifnot(
      "Only one filter set can be used with aggregation" = aggregate_none
    )

    multi_request <- multi_req(fb_records_obj)

    return(multi_request)

  }

  filter <- c(taxa, filter)

  fb_records_obj[["filter"]] <- filter

  include_facts <- !is.null(facts)

  fb_records_obj[["include_facts"]] <- include_facts

  needs_n <- n < 0

  needs_n <- needs_n || is.factor(n)

  needs_n <- needs_n || !is.finite(n)

  if (needs_n) {

    max_page_size <- getOption("finbif_max_page_size")

    fb_records_obj[["n"]] <- max_page_size

    n <- records(fb_records_obj)

    n <- attr(n, "nrec_avl")

    n <- pmax(n, max_page_size)

    fb_records_obj[["n"]] <- n

  }

  records <- records(fb_records_obj)

  if (count_only) {

    total <- c("content", "total")

    ans <- records[[total]]

    return(ans)

  }

  select_user <- attr(records, "select_user", TRUE)

  nrec_dnld <- attr(records, "nrec_dnld", TRUE)

  nrec_avl <- attr(records, "nrec_avl", TRUE)

  date_time <- attr(records, "date_time", TRUE)

  # Don't need a processing progress bar if only one page of records

  quiet <- quiet || length(records) < 2L

  pb_head("Processing data", quiet = quiet)

  fb_occurrence_df <- as.data.frame(records, locale = locale, quiet = quiet)

  colnames <- names(fb_occurrence_df)

  n_col_nms <- grep("^n_", colnames, value = TRUE)

  ind <- !colnames %in% n_col_nms

  vtype <- col_type_string(dwc)

  non_count_cols <- colnames[ind]

  non_count_cols <- var_names[non_count_cols, vtype]

  colnames[ind] <- non_count_cols

  names(fb_occurrence_df) <- colnames

  if (aggregate_counts) {

    select_user <- c(select_user, n_col_nms)

  }

  select_user <- name_chr_vec(select_user)

  df_attrs <- attributes(fb_occurrence_df)

  new_attrs <- list(
    nrec_dnld = nrec_dnld,
    nrec_avl  = nrec_avl,
    column_names = select_user,
    aggregate = aggregate,
    dwc = dwc,
    date_time = date_time,
    date_time_method = date_time_method,
    tzone = tzone,
    locale = locale,
    include_new_cols = TRUE,
    facts = facts,
    unlist = unlist,
    drop_na = drop_na
  )

  df_attrs <- c(df_attrs, new_attrs)

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

  fb_occurrence_df <- extract_facts(fb_occurrence_df)

  class <- c("finbif_occ", "data.frame")

  class(fb_occurrence_df) <- class

  facts <- as.character(facts)

  facts <- name_chr_vec(facts)

  select_user <- c(select_user, facts)

  fb_occurrence_df <- fb_occurrence_df[select_user]

  attr(fb_occurrence_df, "column_names") <- select_user

  fb_occurrence_df <- unlist_cols(fb_occurrence_df)

  fb_occurrence_df <- drop_na_col(fb_occurrence_df)

  names(fb_occurrence_df) <- names(select_user)

  fb_occurrence_df

}

#' @noRd

det_datetime_method <- function(fb_records_obj) {

  n <- fb_records_obj[["n"]]

  method <- fb_records_obj[["date_time_method"]]

  is_null <- is.null(method)

  if (is_null) {

    method <- "none"

    is_num <- is.numeric(n)

    is_pos <- n >= 0L

    cond <- is_num & is_pos

    n <- ifelse(cond, n, Inf)

    n <- sum(n)

    n_small <- n < 1e5L

    if (n_small) {

      method <- "fast"

    }

  }

  fb_records_obj[["date_time_method"]] <- method

  fb_records_obj

}

#' @noRd

select_taxa <- function(fb_records_obj) {

  taxa <- fb_records_obj[["taxa"]]

  cache <- fb_records_obj[["cache"]]

  check_taxa <- fb_records_obj[["check_taxa"]]

  on_check_fail <- fb_records_obj[["on_check_fail"]]

  ntaxa <- length(taxa)

  has_taxa <- ntaxa > 0L

  if (has_taxa) {

    taxon_name <- paste(taxa, collapse = ",")

    ans <- list(taxon_name = taxon_name)

    if (check_taxa) {

      taxa_names <- names(taxa)

      no_taxa_name <- !"taxa" %in% taxa_names

      cond <- no_taxa_name || ntaxa > 1L

      if (cond) {

        taxa <- finbif_check_taxa(taxa, cache = cache)

      } else {

        cache <- list(cache = cache)

        taxa <- as.list(taxa)

        taxa <- c(taxa, cache)

        taxa <- do.call(finbif_check_taxa, taxa)

      }

      taxa <- unlist(taxa)

      taxa_invalid <- is.na(taxa)

      any_taxa_invalid <- any(taxa_invalid)

      if (any_taxa_invalid) {

        invalid_taxa <- taxa[taxa_invalid]

        invalid_taxa_names <- names(invalid_taxa)

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
          on_check_fail,
          warn  = warning(msg, call. = FALSE),
          error = stop(msg, call. = FALSE)
        )

      }

      taxa_valid <- !taxa_invalid

      any_taxa_valid <- any(taxa_valid)

      if (any_taxa_valid) {

        valid_taxa <- taxa[taxa_valid]

        valid_taxa <- paste(valid_taxa, collapse = ",")

        ans <- list(taxon_id = valid_taxa)

      }

    }

    fb_records_obj[["taxa"]] <- ans

  }

  fb_records_obj

}

#' @noRd

date_times <- function(fb_occurrence_df) {

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

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

  date_time <- attr(fb_occurrence_df, "date_time", TRUE)

  if (date_time) {

    date_start <- fb_occurrence_df[[date_start]]

    hour_start <- fb_occurrence_df[[hour_start]]

    minute_start <- fb_occurrence_df[[minute_start]]

    date_end <- fb_occurrence_df[[date_end]]

    hour_end <- fb_occurrence_df[[hour_end]]

    minute_end <- fb_occurrence_df[[minute_end]]

    lat <- fb_occurrence_df[[lat]]

    lon <- fb_occurrence_df[[lon]]

    date_time_start <- list(
      date = date_start,
      hour = hour_start,
      minute = minute_start,
      lat = lat,
      lon = lon,
      tzone = tzone,
      method = method
    )

    date_time_end <- list(
      date = date_end,
      hour = hour_end,
      minute = minute_end,
      lat = lat,
      lon = lon,
      tzone = tzone,
      method = method
    )

    date_time_start <- date_time(date_time_start)

    date_time_end <- date_time(date_time_end)

    df_attrs <- attributes(fb_occurrence_df)

    new_attrs <- list(
      date_time_start = date_time_start,
      date_time_end = date_time_end
    )

    df_attrs <- c(df_attrs, new_attrs)

    attributes(fb_occurrence_df) <- df_attrs

  }

  fb_occurrence_df

}

#' @noRd
#' @importFrom lubridate as_datetime force_tz hour minute with_tz ymd
#' @importFrom lutz tz_lookup_coords

date_time <- function(date_time_obj) {

  tzone <- date_time_obj[["tzone"]]

  date_time <- character()

  date_time <- as.POSIXct(date_time, tz = tzone)

  date <- date_time_obj[["date"]]

  date_length <- length(date)

  date_has_length <- date_length > 0L

  if (date_has_length) {

    date_time <- lubridate::ymd(date)

    date_time <- lubridate::as_datetime(date_time)

    # When there is no hour assume the hour is midday (i.e., don't assume
    # midnight)

    hour <- date_time_obj[["hour"]]

    has_hour <- !is.null(hour)

    if (has_hour) {

      hour_is_na <- is.na(hour)

      date_time_hour <- ifelse(hour_is_na, 12L, hour)

    }

    lubridate::hour(date_time) <- date_time_hour

    minute <- date_time_obj[["minute"]]

    has_minute <- !is.null(minute)

    if (has_minute) {

      date_time_minute <- lubridate::minute(date_time)

      minute_is_na <- is.na(minute)

      date_time_minute <- ifelse(minute_is_na, date_time_minute, minute)

      lubridate::minute(date_time) <- date_time_minute

    }

    method <- date_time_obj[["method"]]

    no_method <- identical(method, "none")

    if (no_method) {

      tz_in <- "Europe/Helsinki"

      date_time <- lubridate::force_tz(date_time, tz_in)

      date_time <- lubridate::with_tz(date_time, tzone)

    } else {

      lat <- date_time_obj[["lat"]]

      lon <- date_time_obj[["lon"]]

      tz_in <- lutz::tz_lookup_coords(lat, lon, method, FALSE)

      tz_in_is_na <- is.na(tz_in)

      tzones <- ifelse(tz_in_is_na, tzone, tz_in)

      date_time <- lubridate::force_tzs(date_time, tzones, tzone)

    }

    month <- date_time_obj[["month"]]

    is_na_month <- is.na(month)

    day <- date_time_obj[["day"]]

    is_na_day <- is.na(day)

    is_na_month_day <- is_na_month | is_na_day

    na_month_day <- lubridate::as_datetime(NA_integer_, tz = tzone)

    date_time[is_na_month_day] <- na_month_day

  }

  date_time

}

#' @noRd

compute_date_time <- function(fb_occurrence_df) {

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

  date_time_var <- var_names["computed_var_date_time", vtype]

  column_names <- attr(fb_occurrence_df, "column_names", TRUE)

  has_date_time <- date_time_var %in% column_names

  if (has_date_time) {

    date_time_start <- attr(fb_occurrence_df, "date_time_start", TRUE)

    fb_occurrence_df[[date_time_var]] <- date_time_start

  }

  fb_occurrence_df

}

#' @noRd
#' @importFrom lubridate as.interval as.duration interval

compute_duration <- function(fb_occurrence_df) {

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

  duration_var <- var_names["computed_var_duration", vtype]

  column_names <- attr(fb_occurrence_df, "column_names", TRUE)

  has_duration <- duration_var %in% column_names

  if (has_duration) {

    hour_start <- var_names["gathering.hourBegin", vtype]

    hour_end <- var_names["gathering.hourEnd", vtype]

    date_time_start <- attr(fb_occurrence_df, "date_time_start", TRUE)

    date_time_end <- attr(fb_occurrence_df, "date_time_end", TRUE)

    hour_start <- fb_occurrence_df[[hour_start]]

    hour_start_is_na <- is.na(hour_start)

    hour_end <- fb_occurrence_df[[hour_end]]

    hour_end_is_na <- is.na(hour_end)

    hour_is_na <- hour_start_is_na | hour_end_is_na

    date_time_start_is_na <- is.na(date_time_start)

    date_time_end_is_na <- is.na(date_time_end)

    date_time_is_na <- date_time_start_is_na | date_time_end_is_na

    duration_is_na <- hour_is_na | date_time_is_na

    duration_length <- length(duration_is_na)

    interval <- rep_len(NA_integer_, duration_length)

    interval <- lubridate::as.interval(interval)

    date_time_start <- date_time_start[!duration_is_na]

    date_time_end <- date_time_end[!duration_is_na]

    date_time_interval <- lubridate::interval(date_time_start, date_time_end)

    duration <- interval

    interval[!duration_is_na] <- date_time_interval

    zero_interval <- interval == 0

    has_duration <- !duration_is_na & !zero_interval

    date_time_interval <- interval[has_duration]

    duration[has_duration] <- date_time_interval

    duration <- lubridate::as.duration(duration)

    fb_occurrence_df[[duration_var]] <- duration

  }

  fb_occurrence_df

}

#' @noRd
#' @importFrom lubridate as.interval format_ISO8601 interval ymd

compute_iso8601 <- function(fb_occurrence_df) {

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

  iso8601_var <- var_names["computed_var_date_time_ISO8601", vtype]

  column_names <- attr(fb_occurrence_df, "column_names", TRUE)

  has_iso8601 <- iso8601_var %in% column_names

  if (has_iso8601) {

    date_start <- var_names["gathering.eventDate.begin", vtype]

    hour_start <- var_names["gathering.hourBegin", vtype]

    minute_start <- var_names["gathering.minutesBegin", vtype]

    date_end <- var_names["gathering.eventDate.end", vtype]

    hour_end <- var_names["gathering.hourEnd", vtype]

    minute_end <- var_names["gathering.minutesEnd", vtype]

    tzone <- attr(fb_occurrence_df, "tzone", TRUE)

    date_time_start <- attr(fb_occurrence_df, "date_time_start", TRUE)

    date_time_end <- attr(fb_occurrence_df, "date_time_end", TRUE)

    date_time_start_is_na <- is.na(date_time_start)

    date_time_end_is_na <- is.na(date_time_end)

    duration_is_na <- date_time_start_is_na | date_time_end_is_na

    duration_length <- length(duration_is_na)

    iso8601 <- rep_len("1970-01-01/1970-01-01", duration_length)

    iso8601 <- lubridate::interval(iso8601, tzone = tzone)

    start_not_na <- date_time_start[!date_time_start_is_na]

    end_not_na <- date_time_end[!date_time_end_is_na]

    iso8601_not_na <- lubridate::interval(start_not_na, end_not_na)

    iso8601_na <- lubridate::as.interval(NA_integer_)

    iso8601[!duration_is_na] <- iso8601_not_na

    iso8601[duration_is_na] <- iso8601_na

    iso8601 <- lubridate::format_ISO8601(iso8601, usetz = TRUE)

    hour_start <- fb_occurrence_df[[hour_start]]

    minute_start <- fb_occurrence_df[[minute_start]]

    hour_end <- fb_occurrence_df[[hour_end]]

    minute_end <- fb_occurrence_df[[minute_end]]

    hour_start_is_na <- is.na(hour_start)

    minute_start_is_na <- is.na(minute_start)

    no_start_time <- hour_start_is_na & minute_start_is_na

    hour_end_is_na <- is.na(hour_end)

    minute_end_is_na <- is.na(minute_end)

    no_end_time <-  hour_end_is_na & minute_end_is_na

    no_time <- no_start_time | no_end_time

    date_start <- fb_occurrence_df[[date_start]]

    date_end <- fb_occurrence_df[[date_end]]

    date_start_ymd <- lubridate::ymd(date_start)

    date_end_ymd <- lubridate::ymd(date_end)

    interval <- lubridate::interval(date_start_ymd, date_end_ymd)

    format_interval <- lubridate::format_ISO8601(interval, usetz = TRUE)

    iso8601 <- ifelse(no_time, format_interval, iso8601)

    no_duration <- date_time_start == date_time_end

    dates_equal <- date_start == date_end

    date_end_is_na <- is.na(date_end)

    no_end_date <- date_end_is_na | dates_equal

    no_end <- no_end_time & no_end_date

    use_start <- no_end | no_duration

    format_start <- lubridate::format_ISO8601(date_time_start, usetz = TRUE)

    iso8601 <- ifelse(use_start, format_start, iso8601)

    use_start_ymd <- no_start_time & no_end_date

    format_start_ymd <- lubridate::format_ISO8601(date_start_ymd, usetz = TRUE)

    iso8601 <- ifelse(use_start_ymd, format_start_ymd, iso8601)

    iso8601_is_na <- is.na(iso8601)

    date_interval <- paste(date_start, date_end, sep = "/")

    date_interval <- ifelse(dates_equal, date_start, date_interval)

    iso8601 <- ifelse(iso8601_is_na, date_interval, iso8601)

    fb_occurrence_df[[iso8601_var]] <- iso8601

  }

  fb_occurrence_df

}

#' @noRd

compute_vars_from_id <- function(fb_occurrence_df) {

  select_user <- attr(fb_occurrence_df, "column_names")

  dwc <- attr(fb_occurrence_df, "dwc")

  locale <- attr(fb_occurrence_df, "locale")

  add <- attr(fb_occurrence_df, "include_new_cols")

  colnames <- names(fb_occurrence_df)

  cols <- setdiff(select_user, colnames)

  vtype <- col_type_string(dwc)

  suffix <- switch(vtype, translated_var = "_id", dwc = "ID")

  sq <- seq_along(cols)

  for (i in sq) {

    col_i <- cols[[i]]

    id_var_name <- paste0(col_i, suffix)

    add_i <- add && id_var_name %in% colnames

    if (add_i) {

      is_collection <- identical(id_var_name, "collection_id")

      if (is_collection) {

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

        metadata <- get(col_i_native)

        not_df <- !inherits(metadata, "data.frame")

        if (not_df) {

          rownames <- lapply(metadata, row.names)

          rownames <- unlist(rownames)

          args <- c(metadata, make.row.names = FALSE)

          metadata <- do.call(rbind, args)

          row.names(metadata) <- rownames

        }

      }

      id_var <- fb_occurrence_df[[id_var_name]]

      nms <- names(metadata)

      ind <- grep(ptrn, nms)

      nms <- gsub(ptrn, "", nms)

      names(metadata) <- nms

      id <- lapply(id_var, vapply, remove_domain, "")

      metadata <- metadata[, ind, drop = FALSE]

      var <- lapply(id, get_rows, metadata)

      var <- lapply(var, apply, 1L, with_locale, locale)

      var <- lapply(var, unname)

      var_is_na <- lapply(var, is.na)

      df_col_i <- mapply(
        ifelse, var_is_na, id_var, var, SIMPLIFY = FALSE, USE.NAMES = FALSE
      )

      unlist <- !is.list(id_var)

      if (unlist) {

        df_col_i <- unlist(df_col_i)

      }

      fb_occurrence_df[[col_i]] <- df_col_i

    }

  }

  fb_occurrence_df

}

#' @noRd

compute_epsg <- function(fb_occurrence_df) {

  add <- attr(fb_occurrence_df, "include_new_cols", TRUE)

  if (add) {

    select_user <- attr(fb_occurrence_df, "column_names", TRUE)

    dwc <- attr(fb_occurrence_df, "dwc", TRUE)

    vtype <- col_type_string(dwc)

    var_names_type <- var_names[, vtype]

    select_user <- match(select_user, var_names_type)

    select_user <- var_names[select_user, ]

    select_user <- row.names(select_user)

    crs <- c(computed_var_epsg = "", computed_var_fp_epsg = "footprint")

    crs_nms <- names(crs)

    sq <- seq_along(crs)

    for (i in sq) {

      epsg <- c("euref", "ykj", "wgs84")

      names(epsg) <- epsg

      crs_i <- crs[[i]]

      crs_nm_i <- crs_nms[[i]]

      epsg[] <- paste0(crs_i, "_", epsg, "$")

      var_names_i <- var_names[select_user, "translated_var"]

      epsg <- lapply(epsg, grepl, var_names_i)

      epsg <- lapply(epsg, c, TRUE)

      epsg <- lapply(epsg, which)

      epsg <- vapply(epsg, min, 0L, USE.NAMES = TRUE)

      epsg <- which.min(epsg)

      epsg <- names(epsg)

      epsg <- switch(
        epsg,
        euref = "EPSG:3067",
        ykj = "EPSG:2393",
        wgs84 = "EPSG:4326",
        NA_character_
      )

      n <- nrow(fb_occurrence_df)

      epsg <- rep_len(epsg, n)

      crs_nm <- var_names[[crs_nm_i, vtype]]

      fb_occurrence_df[[crs_nm]] <- epsg

    }

  }

  fb_occurrence_df

}

#' @noRd

compute_abundance <- function(fb_occurrence_df) {

  select_user <- attr(fb_occurrence_df, "column_names", TRUE)

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  add <- attr(fb_occurrence_df, "include_new_cols", TRUE)

  vtype <- col_type_string(dwc)

  abundance_var <- var_names[["computed_var_abundance", vtype]]

  occurrence_status_var <- var_names[["computed_var_occurrence_status", vtype]]

  has_abundance_var <- abundance_var %in% select_user

  has_occurrence_var <- occurrence_status_var %in% select_user

  has_abundance_vars <- has_abundance_var || has_occurrence_var

  add <- add && has_abundance_vars

  if (add) {

    count_var <- var_names[["unit.interpretations.individualCount", vtype]]

    count <- fb_occurrence_df[[count_var]]

    count_one <- count == 1L

    verbatim_var <- var_names[["unit.abundanceString", vtype]]

    verbatim <- fb_occurrence_df[[verbatim_var]]

    has_one <- grepl("1", verbatim)

    has_one <- ifelse(has_one, 1L, NA_integer_)

    abundance <- ifelse(count_one, has_one, count)

    if (has_abundance_var) {

      fb_occurrence_df[[abundance_var]] <- abundance

    }

    if (has_occurrence_var) {

      fi <- c("paikalla", "poissa")

      sv <- c("n\u00e4rvarande", "fr\u00e5nvarande")

      en <- c("present", "absent")

      locale <- attr(fb_occurrence_df, "locale", TRUE)

      status <- switch(locale, fi = fi, sv = sv, en)

      present <- status[[1L]]

      absent <- status[[2L]]

      abundance_is_na <- is.na(abundance)

      is_present <- abundance > 0L

      is_present <- abundance_is_na | is_present

      occurrence_status <- ifelse(is_present, present, absent)

      fb_occurrence_df[[occurrence_status_var]] <- occurrence_status

    }

  }

  fb_occurrence_df

}

#' @noRd

compute_citation <- function(fb_occurrence_df) {

  select_user <- attr(fb_occurrence_df, "column_names", TRUE)

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  add <- attr(fb_occurrence_df, "include_new_cols", TRUE)

  vtype <- col_type_string(dwc)

  citation_var <- var_names[["computed_var_citation", vtype]]

  add <- add && citation_var %in% select_user

  if (add) {

    record_id <- attr(fb_occurrence_df, "record_id", TRUE)

    source_var <- var_names[["document.sourceId", vtype]]

    source <- fb_occurrence_df[[source_var]]

    source_is_kotka <- source == "http://tun.fi/KE.3"

    document_id_var <- var_names[["document.documentId", vtype]]

    document_id <- fb_occurrence_df[[document_id_var]]

    citation <- ifelse(source_is_kotka, document_id, record_id)

    citation <- paste(citation, "Source: FinBIF")

    fb_occurrence_df[[citation_var]] <- citation

  }

  fb_occurrence_df

}

#' @noRd

compute_coordinate_uncertainty <- function(fb_occurrence_df) {

  select_user <- attr(fb_occurrence_df, "column_names", TRUE)

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  add <- attr(fb_occurrence_df, "include_new_cols", TRUE)

  vtype <- col_type_string(dwc)

  coord_uncert_var <- var_names[["computed_var_coordinates_uncertainty", vtype]]

  add <- add && coord_uncert_var %in% select_user

  if (add) {

    interpreted_var <- "gathering.interpretations.coordinateAccuracy"

    interpreted_var <- var_names[[interpreted_var, vtype]]

    interpreted <- fb_occurrence_df[[interpreted_var]]

    source_var <- var_names[["document.sourceId", vtype]]

    source <- fb_occurrence_df[[source_var]]

    source_is_kotka <- source == "http://tun.fi/KE.3"

    interpreted_one <- interpreted == 1

    is_na <- source_is_kotka & interpreted_one

    coord_uncert <- ifelse(is_na, NA_real_, interpreted)

    coord_uncert <- as.numeric(coord_uncert)

    fb_occurrence_df[[coord_uncert_var]] <- coord_uncert

  }

  fb_occurrence_df

}

#' @noRd

compute_scientific_name <- function(fb_occurrence_df) {

  select_user <- attr(fb_occurrence_df, "column_names", TRUE)

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  add <- attr(fb_occurrence_df, "include_new_cols", TRUE)

  vtype <- col_type_string(dwc)

  sci_name_var <- var_names[["computed_var_scientific_name", vtype]]

  add <- add && sci_name_var %in% select_user

  if (add) {

    sci_name_interp <- var_names[["unit.linkings.taxon.scientificName", vtype]]

    sci_name_interps <- fb_occurrence_df[[sci_name_interp]]

    verbatim <- var_names[["unit.taxonVerbatim", vtype]]

    verbatims <- fb_occurrence_df[[verbatim]]

    author <- var_names[["unit.linkings.taxon.scientificNameAuthorship", vtype]]

    authors <- fb_occurrence_df[[author]]

    verbatim_author <- var_names[["unit.author", vtype]]

    verbatim_authors <- fb_occurrence_df[[verbatim_author]]

    source_var <- var_names[["document.sourceId", vtype]]

    source <- fb_occurrence_df[[source_var]]

    source_is_kotka <- source == "http://tun.fi/KE.3"

    unlinked <- is.na(sci_name_interps)

    use_verbatim <- source_is_kotka & unlinked

    with_verbatim <- list(names = verbatims, authors = verbatim_authors)

    with_verbatim <- add_authors(with_verbatim)

    without_verbatim <- list(names = sci_name_interps, authors = authors)

    without_verbatim <- add_authors(without_verbatim)

    sci_name <- ifelse(use_verbatim, with_verbatim, without_verbatim)

    fb_occurrence_df[[sci_name_var]] <- sci_name

  }

  fb_occurrence_df

}

#' @noRd

add_authors <- function(names_obj) {

  names <- names_obj[["names"]]

  authors <- names_obj[["authors"]]

  nchars <- nchar(authors)

  has_chars <- nchars > 0L

  has_authors <- !is.na(authors)

  has_authors <- has_authors & has_chars

  authors <- paste0(" ", authors)

  authors <- ifelse(has_authors, authors, "")

  with_authors <- paste0(names, authors)

  names_na <- is.na(names)

  ifelse(names_na, names, with_authors)

}

#' @noRd

compute_red_list_status <- function(fb_occurrence_df) {

  select_user <- attr(fb_occurrence_df, "column_names", TRUE)

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  add <- attr(fb_occurrence_df, "include_new_cols", TRUE)

  vtype <- col_type_string(dwc)

  red_list_var <- var_names[["computed_var_red_list_status", vtype]]

  add <- add && red_list_var %in% select_user

  if (add) {

    red_list_id <- "unit.linkings.taxon.latestRedListStatusFinland.status"

    red_list_id <- var_names[[red_list_id, vtype]]

    red_list_id <- fb_occurrence_df[[red_list_id]]

    red_list_year <- "unit.linkings.taxon.latestRedListStatusFinland.year"

    red_list_year <- var_names[[red_list_year, vtype]]

    red_list_year <- fb_occurrence_df[[red_list_year]]

    red_list_na <- is.na(red_list_id)

    red_list_id <- sub("http://tun.fi/MX.iucn", "", red_list_id)

    red_list_id <- paste(red_list_id, red_list_year)

    red_list_id <- ifelse(red_list_na, NA_character_, red_list_id)

    fb_occurrence_df[[red_list_var]] <- red_list_id

  }

  fb_occurrence_df

}

#' @noRd

compute_region <- function(fb_occurrence_df) {

  select_user <- attr(fb_occurrence_df, "column_names", TRUE)

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  add <- attr(fb_occurrence_df, "include_new_cols", TRUE)

  vtype <- col_type_string(dwc)

  region_var <- var_names[["computed_var_region", vtype]]

  add <- add && region_var %in% select_user

  if (add) {

    municipality_id <- "gathering.interpretations.finnishMunicipality"

    municipality_id <-  var_names[[municipality_id, vtype]]

    municipality_id <- fb_occurrence_df[[municipality_id]]

    municipality_id <- basename(municipality_id)

    region <- municipality[municipality_id, "region"]

    fb_occurrence_df[[region_var]] <- region

  }

  fb_occurrence_df

}

#' @noRd

multi_req <- function(fb_records_obj) {

  filters <- fb_records_obj[["filter"]]

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

    fb_records_obj_arg <- fb_records_obj[[arg]]

    fb_records_obj_arg <- rep_len(fb_records_obj_arg, n_filters)

    fb_records_obj[[arg]] <- fb_records_obj_arg

  }

  fb_records_obj_filter <- fb_records_obj

  fb_records_obj_filter[["check_taxa"]] <- FALSE

  filter_sq <- seq_len(n_filters)

  for (filter in filter_sq) {

    args <- c("filter", rep_args)

    for (arg in args) {

      fb_records_obj_arg <- fb_records_obj[[arg]]

      fb_records_obj_arg_filter <- fb_records_obj_arg[[filter]]

      fb_records_obj_filter[[arg]] <- fb_records_obj_arg_filter

    }

    resp <- occurrence(fb_records_obj_filter)

    ans[[filter]] <- resp

  }

  count_only <- fb_records_obj[["count_only"]]

  if (!count_only) {

    ans <- do.call(rbind, ans)

    record_id <- attr(ans, "record_id")

    dups <- duplicated(record_id)

    ans <- ans[!dups, ]

  }

  ans

}

#' @noRd

unlist_cols <- function(fb_occurrence_df) {

  unlist <- attr(fb_occurrence_df, "unlist", TRUE)

  if (unlist) {

    cols <- attr(fb_occurrence_df, "column_names", TRUE)

    for (col in cols) {

      fb_occurrence_df_col <- fb_occurrence_df[[col]]

      is_list_col <- is.list(fb_occurrence_df_col)

      is_list_col <- is_list_col && !grepl("Fact|fact_", col)

      if (is_list_col) {

        fb_occurrence_df_col <- vapply(fb_occurrence_df_col, concat_string, "")

        fb_occurrence_df[[col]] <- fb_occurrence_df_col

      }

    }

  }

  fb_occurrence_df

}

#' @noRd

drop_na_col <- function(fb_occurrence_df) {

  drop_which <- attr(fb_occurrence_df, "drop_na", TRUE)

  drop_any <- any(drop_which)

  if (drop_any) {

    ncols <- length(fb_occurrence_df)

    nrows <- nrow(fb_occurrence_df)

    column_names <- attr(fb_occurrence_df, "column_names", TRUE)

    which <- rep_len(drop_which, ncols)

    fb_occurrence_df_attrs <- attributes(fb_occurrence_df)

    cls <- class(fb_occurrence_df)

    fb_occurrence_df_attrs[["class"]] <- cls

    rnms <- seq_len(nrows)

    fb_occurrence_df_attrs[["row.names"]] <- rnms

    attr(fb_occurrence_df, "class") <- "list"

    is_na <- lapply(fb_occurrence_df, is.na)

    is_na <- vapply(is_na, all, NA)

    drop <- is_na & which

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

  has_facts <- !is.null(facts)

  if (has_facts) {

    dwc <- attr(fb_occurrence_df, "dwc", TRUE)

    vtype <- col_type_string(dwc)

    nms <- c("unit.facts.fact", "gathering.facts.fact", "document.facts.fact")

    vls <- c(
      "unit.facts.value", "gathering.facts.value", "document.facts.value"
    )

    fb_occurrence_df[facts] <- NA_character_

    levels <- seq_len(3L)

    for (fact in facts) {

      for (level in levels) {

        levels_nms <- nms[[level]]

        fact_name <- var_names[[levels_nms, vtype]]

        fact_col <- fb_occurrence_df[[fact_name]]

        is_fact <- lapply(fact_col, "==", fact)

        level_vls <- vls[[level]]

        values_name <- var_names[[level_vls, vtype]]

        values_col <- fb_occurrence_df[[values_name]]

        fact_values <- mapply(extract_fact, values_col, is_fact)

        fact_value_is_na <- is.na(fact_values)

        has_value <- !all(fact_value_is_na)

        if (has_value) {

          fb_occurrence_df[[fact]] <- fact_values

        }

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

  xi <- x[i]

  l <- length(xi)

  is_na <- identical(l, 0L)

  if (is_na) {

    xi <- NA_character_

  }

  xi

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

  res <- finbif_occurrence(
    ..., filter = filter, select = "load_date", order_by = "-load_date", n = 1L
  )

  ans <- character()

  ans <- as.Date(ans)

  nrows <- nrow(res)

  has_rows <- nrows > 0L

  if (has_rows) {

    ans <- res[["load_date"]]

    ans <- ans[[1L]]

    ans <- as.Date(ans)

  }

  ans

}
