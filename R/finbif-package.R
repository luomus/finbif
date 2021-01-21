#' @section Package options:
#' \describe{
#' \item{`finbif_api_url`}{Character. The base url of the API to query. Default:
#'   `"api.laji.fi"`}
#' \item{`finbif_api_version`}{Character. The API version to use. Default:
#'   `"v0"`}
#' \item{`finbif_use_cache`}{Logical. Should API queries by cached. Default:
#'   `TRUE`}
#' \item{`finbif_cache_path`}{Character. The path to the directory where to
#'   store cached API queries. If unset (the default) in memory caching is
#'   used.}
#' \item{`finbif_tz`}{Character. The timezone used by `finbif` functions that
#'   compute dates and times. Default: `Sys.timezone()`}
#' \item{`finbif_locale`}{Character. One of the supported two-letter ISO 639-1
#'   language codes. Current supported languages are English, Finnish, Swedish,
#'   Russian, and Sámi (Northern). By default, the system settings are used to
#'   set this option if they are set to one of the supported languages,
#'   otherwise English is used.}
#' }
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
