#' FinBIF metadata
#'
#' Display metadata from the FinBIF database.
#'
#' @aliases fb_metadata
#'
#' @param which Character. Which category of metadata to display. If
#'   unspecified, function returns the categories of metadata available.
#' @param locale Character. One of the supported two-letter ISO 639-1 language
#'   codes. Current supported languages are English, Finnish and Swedish. For
#'   data where more than one language is available the language denoted by
#'   `locale` will be preferred while falling back to the other languages in the
#'   order indicated above.
#' @param cache Logical or Integer. If `TRUE` or a number greater than zero,
#'   then data-caching will be used. If not logical then cache will be
#'   invalidated after the number of hours indicated by the argument.
#'
#' @return A data.frame.
#' @examples \dontrun{
#'
#' finbif_metadata("red_list")
#'
#' }
#' @export
finbif_metadata <- function(
  which,
  locale = getOption("finbif_locale"),
  cache = getOption("finbif_use_cache_metadata")
) {
  metadata_name <- c(
    "regulatory_status",
    "red_list",
    "country",
    "region",
    "bio_province",
    "finnish_municipality",
    "bird_assoc_area",
    "finnish_occurrence_status",
    "habitat_type",
    "habitat_qualifier",
    "life_stage",
    "record_basis",
    "restriction_level",
    "restriction_reason",
    "sex_category",
    "source",
    "taxon_rank"
  )
  data <- data.frame(metadata_name)

  if (!missing(which)) {
    if (!which %in% metadata_name) {
      stop(which, " not found in FinBIF metadata.", call. = FALSE)
    }

    obj <- list(cache = cache)
    obj[["which"]] <- switch(
      which,
      red_list = "red_list_status",
      sex_category = "sex",
      habitat_type = "primary_habitat",
      habitat_qualifier = "primary_habitat",
      which
    )
    data <- sysdata(obj)
    data <- switch(
      which,
      habitat_type = data[["habitat_types"]],
      habitat_qualifier = data[["specific_habitat_types"]],
      data
    )

    col_names <- "name"
    name <- paste0("name_", locale)

    if (!name %in% names(data)) {
      name <- "name_en"
      locale <- "en"
    }

    cols <- name
    description <- paste0("description_", locale)

    if (description %in% names(data)) {
      cols <- c(cols, description)
      col_names <- c(col_names, "description")
    }

    if ("code" %in% names(data)) {
      cols <- c("code", cols)
      col_names <- c("code", col_names)
    }

    ind <- order(data[[name]])
    data <- data[ind, cols, drop = FALSE]
    colnames(data) <- col_names
    class(data) <- c("finbif_metadata_df", "data.frame")
  }

  data
}
