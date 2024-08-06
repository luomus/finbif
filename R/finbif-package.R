#' @section Package options:
#' \describe{
#' \item{`finbif_api_url`}{Character. The base url of the API to query. Default:
#'   `"https://api.laji.fi"`}
#' \item{`finbif_api_version`}{Character. The API version to use. Default:
#'   `"v0"`}
#' \item{`finbif_allow_query`}{Logical. Should remote API queries by allowed.
#'   Default: `TRUE`}
#' \item{`finbif_use_cache`}{Logical or Integer. If `TRUE` or a number greater
#'   than zero, then data-caching will be used. If not logical then cache will
#'   be invalidated after the number of hours indicated by the value. Default:
#'   `TRUE`}
#' \item{`finbif_use_cache_metadata`}{Logical or Integer. If `TRUE` or a number
#'   greater than zero, then metadata-caching will be used. If not logical then
#'   cache will be invalidated after the number of hours indicated by the value.
#'   Default: `TRUE`}
#' \item{`finbif_cache_path`}{Character. The path to the directory where to
#'   store cached API queries. If unset (the default) in memory caching is
#'   used.}
#' \item{`finbif_tz`}{Character. The timezone used by `finbif` functions that
#'   compute dates and times. Default: `Sys.timezone()`}
#' \item{`finbif_locale`}{Character. One of the supported two-letter ISO 639-1
#'   language codes. Current supported languages are English, Finnish and
#'   Swedish. By default, the system settings are used to set this option if
#'   they are set to one of the supported languages, otherwise English is used.}
#' \item{`finbif_hide_progress`}{Logical. Global option to suppress progress
#'   indicators for downloading, importing and processing FinBIF records.
#'   Default: `FALSE`}
#' }
"_PACKAGE"
