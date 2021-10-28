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
#' @param locale Character. One of the supported two-letter ISO 639-1 language
#'   codes. Current supported languages are English, Finnish, Swedish, Russian,
#'   and Sámi (Northern). For data where more than one language is available
#'   the language denoted by `locale` will be preferred while falling back to
#'   the other languages in the order indicated above.
#' @param drop_na Logical. A vector indicating which columns to check for
#'   missing data. Values recycled to the number of columns. Defaults to all
#'   columns.
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
#' @importFrom methods as
#' @importFrom utils hasName
#' @importFrom lubridate as_datetime as.duration force_tzs format_ISO8601 hour
#' @importFrom lubridate interval minute ymd
#' @importFrom lutz tz_lookup_coords
#' @export

finbif_occurrence <- function(
  ..., filter, select, order_by, aggregate, sample = FALSE, n = 10, page = 1,
  count_only = FALSE, quiet = FALSE, cache = getOption("finbif_use_cache"),
  dwc = FALSE, date_time_method, check_taxa = TRUE,
  on_check_fail = c("warn", "error"), tzone = getOption("finbif_tz"),
  locale = getOption("finbif_locale"), seed, drop_na = FALSE
) {

  taxa <- select_taxa(
    ..., cache = cache, check_taxa = check_taxa,
    on_check_fail = match.arg(on_check_fail)
  )

  date_time_method <- det_datetime_method(date_time_method, n = sum(n))

  if (missing(filter) || is.null(filter)) {

    filter <- NULL

  } else {

    if (is.null(names(filter))) {

      stopifnot(
        "only one filter set can be used with aggregation" = missing(aggregate)
      )

      multi_request <- multi_req(
        taxa, filter, select, order_by, sample, n, page, count_only, quiet,
        cache, dwc, date_time_method, tzone, locale
      )

      return(multi_request)

    }

  }

  filter <- c(taxa, filter)

  records <- finbif_records(
    filter, select, order_by, aggregate, sample, n, page, count_only, quiet,
    cache, dwc, df = TRUE, seed
  )

  aggregate <- attr(records, "aggregate", TRUE)

  if (count_only) return(records[["content"]][["total"]])

  # Don't need a processing progress bar if only one page of records
  quiet <- quiet || length(records) < 2L

  pb_head("Processing data", quiet = quiet)

  df   <- as.data.frame(records, locale = locale, quiet = quiet)
  url  <- attr(df, "url", TRUE)
  time <- attr(df, "time", TRUE)
  record_id <- attr(df, "record_id", TRUE)

  n_col_nms <- grep("^n_", names(df), value = TRUE)

  ind <- !names(df) %in% n_col_nms

  names(df)[ind] <- var_names[names(df)[ind], col_type_string(dwc)]

  select_ <- attr(records, "select_user")

  select_ <-  name_chr_vec(c(select_, n_col_nms))

  df <- compute_date_time(
    df, select, select_, aggregate, dwc, date_time_method, tzone
  )

  df <- compute_vars_from_id(df, select_, dwc, locale)

  df <- compute_epsg(df, select_, dwc)

  df <- compute_abundance(df, select_, dwc)

  df <- coordinates_uncertainty(df, select_, dwc)

  df <- structure(
    df[select_],
    class     = c("finbif_occ", "data.frame"),
    nrec_dnld = attr(records, "nrec_dnld", TRUE),
    nrec_avl  = attr(records, "nrec_avl", TRUE),
    url       = url,
    time      = time,
    dwc       = dwc,
    column_names = select_,
    record_id = record_id
  )

  names(df) <- names(select_)

  drop_na_col(df, drop_na)

}

#' @noRd

select_taxa <- function(..., cache, check_taxa, on_check_fail) {

  taxa <- c(...)
  ntaxa <- length(taxa)

  if (identical(ntaxa, 0L)) return(NULL)

  ans <- list(taxon_name = paste(taxa, collapse = ","))

  if (check_taxa) {

    if (ntaxa > 1L || !utils::hasName(taxa, "taxa")) {
      taxa <- unlist(finbif_check_taxa(taxa, cache = cache))
    } else {
      taxa <- unlist(finbif_check_taxa(..., cache = cache))
    }

    taxa_invalid <- is.na(taxa)
    taxa_valid  <- !taxa_invalid

    if (any(taxa_invalid)) {
      msg  <- paste(
        "Cannot find taxa:",
        paste(sub("\\.", " - ", names(taxa[taxa_invalid])), collapse = ", ")
      )
      switch(
        on_check_fail,
        warn  = warning(msg, call. = FALSE),
        error = stop(msg, call. = FALSE)
      )
    }

    if (any(taxa_valid)) {
      ans <- list(taxon_id = paste(taxa[taxa_valid], collapse = ","))
    }

  }

  ans

}

#' @noRd

compute_date_time <- function(
  df, select, select_, aggregate, dwc, date_time_method, tzone
) {

  vars <- c(
    "date_time", "eventDateTime", "date_time_ISO8601", "eventDate",
    "duration", "samplingEffort"
  )

  if (missing(select)) {

    date_time <- identical("none", aggregate)

  } else {

    if (identical("none", aggregate)) vars <- c(vars, "default_vars")
    date_time <- any(vars %in% select)

  }

  if (date_time) {
    if (dwc) {
      df[["eventDateTime"]] <- get_date_time(
        df, "eventDateStart", "hourStart", "minuteStart",
        "decimalLatitude", "decimalLongitude", date_time_method, tzone
      )
      if ("samplingEffort" %in% select_) {
        df[["samplingEffort"]] <- get_duration(
          df, "eventDateTime", "eventDateEnd", "hourEnd",
          "minuteEnd", "decimalLatitude", "decimalLongitude",
          date_time_method, tzone
        )
      }
      if ("eventDate" %in% select_) {
        df[["eventDate"]] <- get_iso8601(
          df, "eventDateTime", "eventDateStart", "hourStart", "minuteStart",
          "eventDateEnd", "hourEnd", "minuteEnd", "decimalLatitude",
          "decimalLongitude", date_time_method, tzone
        )
      }
    } else {
      df[["date_time"]] <- get_date_time(
        df, "date_start", "hour_start", "minute_start", "lat_wgs84",
        "lon_wgs84", date_time_method, tzone
      )
      if ("duration" %in% select_) {
        df[["duration"]] <- get_duration(
          df, "date_time", "date_end", "hour_end", "minute_end", "lat_wgs84",
          "lon_wgs84", date_time_method, tzone
        )
      }
      if ("date_time_ISO8601" %in% select_) {
        df[["date_time_ISO8601"]] <- get_iso8601(
          df, "date_time", "date_start", "hour_start", "minute_start",
          "date_end", "hour_end", "minute_end", "lat_wgs84", "lon_wgs84",
          date_time_method, tzone
        )
      }
    }
  }

  df

}

#' @noRd

get_date_time <- function(df, date, hour, minute, lat, lon, method, tzone) {

  date_time <- lubridate::ymd(df[[date]])
  date_time <- lubridate::as_datetime(date_time)

  # When there is no hour assume the hour is midday (i.e., don't assume
  # midnight)
  lubridate::hour(date_time) <- 12L

  if (!is.null(df[[hour]])) {
    lubridate::hour(date_time) <- ifelse(
      is.na(df[[hour]]), lubridate::hour(date_time), df[[hour]]
    )
  }

  if (!is.null(df[[minute]])) {
    lubridate::minute(date_time) <- ifelse(
      is.na(df[[minute]]), lubridate::minute(date_time), df[[minute]]
    )
  }

  method <- match.arg(method, c("none", "fast", "accurate"), TRUE)

  if (identical(method, "none")) {

    tz_in <- "Europe/Helsinki"
    date_time <- lubridate::force_tz(date_time, tz_in)
    lubridate::with_tz(date_time, tzone)

  } else {

    tz_in <- lutz::tz_lookup_coords(df[[lat]], df[[lon]], method, FALSE)
    lubridate::force_tzs(
      date_time, tzones = ifelse(is.na(tz_in), tzone, tz_in), tzone_out = tzone
    )

  }

}

#' @noRd

get_duration <- function(
  df, date_time, date, hour, minute, lat, lon, method, tzone
) {

  date_time_end <- get_date_time(
    df, date, hour, minute, lat, lon, method, tzone
  )

  ans <- lubridate::interval(df[[date_time]], date_time_end)
  missing_interval <- lubridate::interval(NA_character_)
  ans <- ifelse(is.na(df[[minute]]) | is.na(df[[hour]]), missing_interval, ans)
  lubridate::as.duration(ans)

}

#' @noRd

get_iso8601 <- function(
  df, date_time, date_start, hour_start, minute_start, date_end, hour_end,
  minute_end, lat, lon, method, tzone
) {

  date_time_end <- get_date_time(
    df, date_end, hour_end, minute_end, lat, lon, method, tzone
  )

  ans <- lubridate::interval(df[[date_time]], date_time_end)

  ans <- lubridate::format_ISO8601(ans, usetz = TRUE)

  ans <- ifelse(
    is.na(df[[minute_start]]) & is.na(df[[hour_start]]) |
      is.na(df[[minute_end]]) & is.na(df[[hour_end]]),
    lubridate::format_ISO8601(
      lubridate::interval(
        lubridate::ymd(df[[date_start]]), lubridate::ymd(df[[date_end]])
      ),
      usetz = TRUE
    ),
    ans
  )

  ans <- ifelse(
    ((is.na(df[[minute_end]]) & is.na(df[[hour_end]])) &
      (is.na(df[[date_end]]) | df[[date_start]] == df[[date_end]])) |
        df[[date_time]] == date_time_end,
    lubridate::format_ISO8601(df[[date_time]], usetz = TRUE),
    ans
  )

  ans <- ifelse(
    (is.na(df[[minute_start]]) & is.na(df[[hour_start]])) &
      (is.na(df[[date_end]]) | df[[date_start]] == df[[date_end]]),
    lubridate::format_ISO8601(lubridate::ymd(df[[date_start]]), usetz = TRUE),
    ans
  )

  ifelse(is.na(df[[date_start]]), NA_character_, ans)

}

#' @noRd

compute_vars_from_id <- function(df, select_, dwc, locale) {

  candidates <- setdiff(select_, names(df))

  suffix <- switch(col_type_string(dwc), translated_var = "_id", dwc = "ID")

  for (k in seq_along(candidates)) {

    id_var_name <- paste0(candidates[[k]], suffix)

    if (utils::hasName(df, id_var_name)) {

      if (identical(id_var_name, "collection_id")) {

        ptrn <- "collection_name"

        metadata <- finbif_collections(
          select = ptrn, subcollections = TRUE,
          supercollections = TRUE, nmin = NA
        )

      } else {

        ptrn <- "^name_"

        metadata <- get(to_native(candidates[[k]]))

      }

      i <- gsub("http://tun.fi/", "", df[[id_var_name]])

      j <- grep(ptrn, names(metadata))

      var <- metadata[i, j, drop = FALSE]

      names(var) <- gsub(ptrn, "", names(var))

      var <- apply(var, 1L, as.list)

      var <- vapply(var, with_locale, NA_character_, locale)

      df[[candidates[[k]]]] <- ifelse(is.na(var), df[[id_var_name]], var)

    }

  }

  df

}

#' @noRd

compute_epsg <- function(df, select_, dwc) {

  select_ <- var_names[, col_type_string(dwc)] %in% select_

  select_ <- row.names(var_names[select_, ])

  epsg <- c("euref", "kkj", "wgs84")

  names(epsg) <- epsg

  epsg[] <- paste0("_", epsg, "$")

  epsg <- lapply(epsg, grepl, var_names[select_, "translated_var"])

  epsg <- lapply(epsg, c, TRUE)

  epsg <- lapply(epsg, which)

  epsg <- vapply(epsg, min, integer(1L), USE.NAMES = TRUE)

  epsg <- names(which.min(epsg))

  epsg <- switch(
    epsg,
    euref = "EPSG:3067", kkj = "EPSG:2393", wgs84 = "EPSG:4326", NA_character_
  )

  epsg <- rep_len(epsg, nrow(df))

  df[[var_names[["computed_var_epsg", col_type_string(dwc)]]]] <- epsg

  df

}

#' @noRd

compute_abundance <- function(df, select_, dwc) {

  type <- col_type_string(dwc)

  abundance_ <- var_names[["computed_var_abundance", type]]

  abundance_i <- var_names[["unit.interpretations.individualCount", type]]

  abundance_v <- var_names[["unit.abundanceString", type]]

  if (abundance_ %in% select_) {

    abundance <- ifelse(
      df[[abundance_i]] == 1L,
      ifelse(grepl("1", df[[abundance_v]]), 1L, NA_integer_),
      df[[abundance_i]]
    )

    df[[abundance_]] <- abundance

  }

  df

}


#' @noRd

coordinates_uncertainty <- function(df, select_, dwc) {

  type <- col_type_string(dwc)

  coord_uncert_ <- var_names[["computed_var_coordinates_uncertainty", type]]

  coord_uncert_i <- var_names[[
    "gathering.interpretations.coordinateAccuracy", type
  ]]

  source <- var_names[["document.sourceId", type]]

  if (coord_uncert_ %in% select_) {

    coord_uncert <- ifelse(
      df[[source]] == "http://tun.fi/KE.3" & df[[coord_uncert_i]] == 1,
      NA_real_,
      df[[coord_uncert_i]]
    )

    df[[coord_uncert_]] <- coord_uncert

  }

  df

}

#' @noRd

multi_req <- function(
  taxa, filter, select, order_by, sample, n, page, count_only, quiet, cache,
  dwc, date_time_method, tzone, locale
) {

  ans <- vector("list", length(filter))

  rep_args <- c(
    "sample", "n", "page", "quiet", "cache", "date_time_method", "tzone",
    "locale"
  )

  for (arg in rep_args) {

    assign(arg, rep_len(get(arg), length(ans)))

  }

  for (i in seq_along(ans)) {

    ans[[i]] <- finbif_occurrence(
      taxa, filter = filter[[i]], select = select, order_by = order_by,
      sample = sample[[i]], n = n[[i]], page = page[[i]],
      count_only = count_only, quiet = quiet[[i]], cache = cache[[i]],
      dwc = dwc, date_time_method = date_time_method[[i]], check_taxa = FALSE,
      tzone = tzone[[i]], locale = locale[[i]]
    )

  }

  if (!count_only) {

    ans <- do.call(rbind, ans)
    dups <- duplicated(attr(ans, "record_id"))
    ans <- ans[!dups, ]

  }

  ans

}
