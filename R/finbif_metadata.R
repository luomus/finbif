#' FinBIF metadata
#'
#' Display metadata from the FinBIF database.
#'
#' @aliases fb_metadata
#'
#' @param which Character. Which category of metadata to display. If
#'  unspecified, function returns the categories of metadata available.
#'
#' @return A data.frame.
#' @examples \dontrun{
#'
#' finbif_metadata("red_list")
#'
#' }
#' @export

finbif_metadata <- function(
  which
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
      regulatory_status = regulatory_status(),
      red_list = red_list_status(),
      country = country(),
      region = region(),
      bio_province = bio_province(),
      municipality = municipality(),
      bird_assoc_area = bird_assoc_area(),
      finnish_occurrence_status = finnish_occurrence_status(),
      habitat_type = primary_habitat()[["habitat_types"]],
      habitat_qualifier = primary_habitat()[["specific_habitat_types"]],
      life_stage = life_stage(),
      record_basis = record_basis(),
      restriction_level = restriction_level(),
      restriction_reason = restriction_reason(),
      sex_category = sex(),
      source = source(),
      taxon_rank = taxon_rank()
    )

    locale <- getOption("finbif_locale")

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

    rownames(data) <- NULL

    class(data) <- c("finbif_metadata_df", "data.frame")

  }

  data

}
