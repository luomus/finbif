#' Download FinBIF occurrence records
#'
#' Download filtered occurrence data from FinBIF as a `data.frame`.
#'
#' @param ... Character vectors or list of character vectors. Taxa of records
#'   to download.
#' @inheritParams finbif_records
#' @param check_taxa Logical. Check first that taxa are in the FinBIF database.
#'   If true only records that match known taxa (have a valid taxon ID) are
#'   returned.
#' @param date_time Logical. Convert raw date and time fields into date_time and
#'   duration.
#' @param method Character. Passed to `lutz::tz_lookup_coords()` when
#'   `date_time = TRUE`. Default is `"fast"`. Use `method = "accurate"`
#'   (requires package `sf`) for greater accuracy.
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
#'   filters = list(coordinate_accuracy_max = 100)
#' )
#'
#' }
#' @importFrom utils hasName
#' @importFrom lubridate as_datetime as.duration force_tzs hour interval minute
#' @importFrom lubridate ymd
#' @importFrom lutz tz_lookup_coords
#' @export

finbif_occurrence <- function(..., filters, fields, n = 10, page = 1,
  count_only = FALSE, quiet = FALSE, cache = TRUE, check_taxa = TRUE,
  date_time = TRUE, method = "fast"
  ) {

  taxa <- list(...)

  if (check_taxa) {
    taxa <- if (...length() > 1L || !utils::hasName(taxa, "taxa")) {
      finbif_check_taxa(taxa, cache = cache)
    } else {
      finbif_check_taxa(..., cache = cache)
    }
    taxa <- list(taxon_id = paste(unlist(taxa), collapse = ","))
  } else {
    taxa <- list(taxon_name = paste(unlist(taxa), collapse = ","))
  }

  if (missing(filters)) filters <- NULL
  filters <- c(taxa, filters)

  records <- finbif_records(filters, fields, n, page, count_only, quiet, cache)

  if (count_only) return(records[["content"]][["total"]])

  df   <- as.data.frame(records)
  url  <- attr(df, "url", TRUE)
  time <-  attr(df, "time", TRUE)

  df <- df[intersect(row.names(field_names), names(df))]
  names(df) <- field_names[names(df), "translated_field"]

  if (date_time) {
    df$date_time <- get_date_time(
      df, "date_start", "hour_start", "minute_start", "lat_wgs84", "lon_wgs84",
      method
    )
    df$duration <- get_duration(
      df, "date_time", "date_end", "hour_end", "minute_end", "lat_wgs84",
      "lat_wgs84", method
    )
  }

  structure(
    df,
    class     = c("finbif_occ", "data.frame"),
    nrec_dnld = attr(records, "nrec_dnld", TRUE),
    nrec_avl  = attr(records, "nrec_avl", TRUE),
    url       = url,
    time      = time
  )

}

get_date_time <- function(df, date, hour, minute, lat, lon, method) {

  if (is.null(df[[date]])) return(NULL)

  date_time <- lubridate::ymd(df[[date]])
  date_time <- lubridate::as_datetime(date_time)

  if (!is.null(df[[hour]])) {
    lubridate::hour(date_time) <-
      ifelse(is.na(df[[hour]]), lubridate::hour(date_time), df[[hour]])
  }

  if (!is.null(df[[minute]])) {
    lubridate::minute(date_time) <-
      ifelse(is.na(df[[minute]]), lubridate::minute(date_time), df[[minute]])
  }

  if (is.null(df[[lat]]) || is.null(df[[lon]])) return(NULL)
  tz <- lutz::tz_lookup_coords(df[[lat]], df[[lon]], method, FALSE)
  lubridate::force_tzs(date_time, tzones = ifelse(is.na(tz), "", tz))
}

get_duration <- function(df, date_time, date, hour, minute, lat, lon, method) {

  if (is.null(df[[date_time]])) return(NULL)
  if (is.null(df[["date_end"]]) || is.null(df[["hour_end"]])) return(NULL)

  date_time_end <- get_date_time(
    df, "date_end", "hour_end", "minute_end", "lat_wgs84", "lon_wgs84", method
  )

  ans <- lubridate::interval(df[[date_time]], date_time_end)
  lubridate::as.duration(ans)

}
