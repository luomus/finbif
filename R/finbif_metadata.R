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
  locale = getOption("finbif_locale")
) {

  metadata_name <- c(
    "regulatory_status",
    "red_list",
    "country",
    "region",
    "bio_province",
    "municipality",
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

    data <- switch(
      which,
      regulatory_status = sysdata("regulatory_status"),
      red_list = sysdata("red_list_status"),
      country = sysdata("country"),
      region = sysdata("region"),
      bio_province = sysdata("bio_province"),
      municipality = sysdata("municipality"),
      bird_assoc_area = sysdata("bird_assoc_area"),
      finnish_occurrence_status = sysdata("finnish_occurrence_status"),
      habitat_type = sysdata("primary_habitat")[["habitat_types"]],
      habitat_qualifier =
        sysdata("primary_habitat")[["specific_habitat_types"]],
      life_stage = sysdata("life_stage"),
      record_basis = sysdata("record_basis"),
      restriction_level = sysdata("restriction_level"),
      restriction_reason = sysdata("restriction_reason"),
      sex_category = sysdata("sex"),
      source = sysdata("source"),
      taxon_rank = sysdata("taxon_rank")
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
