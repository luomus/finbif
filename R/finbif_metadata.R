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
#' @examples
#' finbif_metadata("red_list")
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

  has_which <- !missing(which)

  ans <- data.frame(metadata_name)

  if (has_which) {

    no_which <- !which %in% metadata_name

    if (no_which) {

      stop(which, " not found in FinBIF metadata.", call. = FALSE)

    }

    ans <- switch(
      which,
      regulatory_status         = md_regulatory_status(),
      red_list                  = md_red_list(),
      country                   = md_countries(),
      region                    = md_regions(),
      bio_province              = md_bio_provinces(),
      municipality              = md_municipalities(),
      bird_assoc_area           = md_bird_assoc_areas(),
      finnish_occurrence_status = md_finnish_occurrence_status(),
      habitat_type              = md_habitat_types(),
      habitat_qualifier         = md_habitat_qualifiers(),
      life_stage                = md_life_stages(),
      record_basis              = md_record_basis(),
      restriction_level         = md_restriction_levels(),
      restriction_reason        = md_restriction_reasons(),
      sex_category              = md_sex_categories(),
      source                    = md_sources(),
      taxon_rank                = md_taxon_ranks()
    )

  }

  class <- c("finbif_metadata_df", "data.frame")

  structure(ans, class = class)

}

#' @noRd

md_regulatory_status <- function() {

  locale <- getOption("finbif_locale")

  col <- paste0("description_", locale)

  col_names <- names(regulatory_status)

  no_locale <- !col %in% col_names

  if (no_locale) {

    col <- "description_en"

  }

  descriptions <- regulatory_status[[col]]

  ind <- order(descriptions)

  cols <- c("status_code", col)

  regulatory_status <- regulatory_status[ind, cols]

  obj <- list(df = regulatory_status, names = cols)

  md(obj)

}

#' @noRd

md_red_list <- function() {

  statuses <- red_list_status[["translated_status"]]

  ind <- order(statuses)

  red_list_status <- red_list_status[ind, ]

  col_names <- c("status_name", "status_code")

  obj <- list(df = red_list_status, names = col_names)

  md(obj)

}

#' @noRd

md_countries <- function() {

  col_names <- c("english_name", "finnish_name", "alpha_code_2", "alpha_code_3")

  obj <- list(df = country, names = col_names)

  md(obj)

}

#' @noRd

md_regions <- function() {

  col_names <- c("english_name", "finnish_name", "swedish_name")

  obj <- list(df = region, names = col_names)

  md(obj)

}

#' @noRd

md_bio_provinces <- function() {

  col_names <- c("english_name", "finnish_name", "alpha_code", "country")

  obj <- list(df = bio_province, names = col_names)

  md(obj)

}

#' @noRd

md_municipalities <- function() {

  col_names <- c("finnish_name", "country")

  obj <- list(df = municipality, names = col_names)

  md(obj)

}

#' @noRd

md_bird_assoc_areas <- function() {

  col_names <- c("finnish_name", "area_code")

  obj <- list(df = bird_assoc_area, names = col_names)

  md(obj)

}

#' @noRd

md_finnish_occurrence_status <- function() {

  col_names <- c("status_description", "status_name")

  obj <- list(df = finnish_occurrence_status, names = col_names)

  md(obj)

}

#' @noRd

md_habitat_types <- function() {

  habitat_types <- primary_habitat[["habitat_types"]]

  locale <- getOption("finbif_locale")

  col <- paste0("name_", locale)

  col_names <- names(habitat_types)

  no_locale <- !col %in% col_names

  if (no_locale) {

    col <- "name_en"

  }

  nms <- habitat_types[[col]]

  ind <- order(nms)

  cols <- c("code", col)

  habitat_types <- habitat_types[ind, cols]

  col_names <- c("habitat_code", "habitat_description")

  obj <- list(df = habitat_types, names = col_names)

  md(obj)

}

#' @noRd

md_habitat_qualifiers <- function() {

  specific_habitat_types <- primary_habitat[["specific_habitat_types"]]

  locale <- getOption("finbif_locale")

  col <- paste0("name_", locale)

  col_names <- names(specific_habitat_types)

  no_locale <- !col %in% col_names

  if (no_locale) {

    col <- "name_en"

  }

  nms <- specific_habitat_types[[col]]

  ind <- order(nms)

  cols <- c("code", col)

  specific_habitat_types <- specific_habitat_types[ind, cols]

  col_names <- c("qualifier_code", "qualifier_description")

  obj <- list(df = specific_habitat_types, names = col_names)

  md(obj)

}

#' @noRd

md_life_stages <- function() {

  col_names <- c("english_name", "finnish_name", "swedish_name")

  obj <- list(df = life_stage, names = col_names)

  md(obj)

}

#' @noRd

md_record_basis <- function() {

  cols <- c("name_en", "name_fi", "name_sv")

  df <- record_basis[cols]

  col_names <- c("english_name", "finnish_name", "swedish_name")

  obj <- list(df = df, names = col_names)

  md(obj)

}

#' @noRd

md_restriction_reasons <- function() {

  col_names <- c(
    "label", "english_description", "finnish_description", "swedish_description"
  )

  obj <- list(df = restriction_reason, names = col_names)

  md(obj)

}

#' @noRd

md_restriction_levels <- function() {

  cols <- c("value", "enumeration")

  df <- restriction_level[cols]

  col_names <- c("level_description", "level_name")

  obj <- list(df = df, names = col_names)

  md(obj)

}

#' @noRd

md_sex_categories <- function() {

  col <- "name_en"

  nms <- sex[[col]]

  ind <- order(nms)

  df <- sex[ind, ]

  col_names <- c("code", "english_name", "finnish_name", "swedish_name")

  obj <- list(df = df, names = col_names)

  md(obj)

}

#' @noRd

md_sources <- function() {

  cols <- c("id", "name_en", "description_en", "name_fi", "description_fi")

  df <- source[cols]

  col_names <- c(
    "source_id",
    "english_name",
    "english_description",
    "finnish_name",
    "finnish_description"
  )

  obj <- list(df = df, names = col_names)

  md(obj)

}

#' @noRd

md_taxon_ranks <- function() {

  obj <- list(df = taxon_rank, names = "rank_name")

  md(obj)

}

#' @noRd

md <- function(obj) {

  df <- obj[["df"]]

  n_rows <- nrow(df)

  row_names <- seq_len(n_rows)

  col_names <- obj[["names"]]

  structure(df, row.names = row_names, names = col_names)

}
