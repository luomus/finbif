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
#' @importFrom utils head
#' @export

finbif_metadata <- function(which) {

  metadata_name <- c(
    "regulatory_status",
    "red_list",
    "country",
    "province",
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


  if (missing(which)) {

    ans <- data.frame(metadata_name)

  } else {

    if (!which %in% metadata_name) {
      stop(which, " not found in FinBIF metadata.")
    }

    ans <- switch(
      which,
      regulatory_status         = md_regulatory_status(),
      red_list                  = md_red_list(),
      country                   = md_countries(),
      province                  = md_provinces(),
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

  class(ans) <- c("finbif_metadata_df", "data.frame")

  ans

}

md_regulatory_status <- function() {

  df <- regulatory_status

  locale <- getOption("finbif_locale")

  col <- paste0("description_", locale)

  if (!col %in% names(df)) {

    col <- "description_en"

  }

  df <- df[order(df[[col]]), c("status_code", col)]

  structure(df, row.names = seq_len(nrow(df)))

}

md_red_list <- function() {
  df <- red_list_status
  df <- df[order(df[["translated_status"]]), ]
  structure(
    df, row.names = seq_len(nrow(df)), names = c("status_name", "status_code")
  )
}

md_countries <- function() {
  structure(
    country,
    row.names = seq_len(nrow(country)),
    names = c("english_name", "finnish_name", "alpha_code_2", "alpha_code_3")
  )
}

md_provinces <- function() {
  structure(
    province, row.names = seq_len(nrow(province)),
    names = c("english_name", "finnish_name", "alpha_code", "country")
  )
}

md_municipalities <- function() {
  structure(
    municipality, row.names = seq_len(nrow(municipality)),
    names = c("finnish_name", "country")
  )
}

md_bird_assoc_areas <- function() {
  structure(
    bird_assoc_area, row.names = seq_len(nrow(bird_assoc_area)),
    names = c("finnish_name", "area_code")
  )
}

md_finnish_occurrence_status <- function() {
  structure(
    finnish_occurrence_status,
    row.names = seq_len(nrow(finnish_occurrence_status)),
    names = c("status_description", "status_name")
  )
}

md_habitat_types <- function() {

  df <- primary_habitat[["habitat_types"]]

  locale <- getOption("finbif_locale")

  col <- paste0("name_", locale)

  if (!col %in% names(df)) {

    col <- "name_en"

  }

  df <- df[order(df[[col]]), c("code", col)]

  structure(
    df, row.names = seq_len(nrow(df)),
    names = c("habitat_code", "habitat_description")
  )

}

md_habitat_qualifiers <- function() {

  df <- primary_habitat[["specific_habitat_types"]]

  locale <- getOption("finbif_locale")

  col <- paste0("name_", locale)

  if (!col %in% names(df)) {

    col <- "name_en"

  }

  df <- df[order(df[[col]]), c("code", col)]

  structure(
    df, row.names = seq_len(nrow(df)),
    names = c("qualifier_code", "qualifier_description")
  )
}

md_life_stages <- function() {
  structure(life_stage, row.names = seq_len(nrow(life_stage)))
}

md_record_basis <- function() {
  structure(
    record_basis[c("description", "name_en")],
    row.names = seq_len(nrow(record_basis)),
    names = c("basis_description", "basis_name")
  )
}

md_restriction_reasons <- function() {
  structure(
    restriction_reason[c("value", "enumeration")],
    row.names = seq_len(nrow(restriction_reason)),
    names = c("reason_description", "reason_name")
  )
}

md_restriction_levels <- function() {
  structure(
    restriction_level[c("value", "enumeration")],
    row.names = seq_len(nrow(restriction_level)),
    names = c("level_description", "level_name")
  )
}

md_sex_categories <- function() {
  structure(
    sex[order(sex[["category"]]), c("category", "code")],
    row.names = seq_len(nrow(sex)),
    names = c("category_name", "category_code")
  )
}

md_sources <- function() {
  structure(
    source[c("id", "name_en", "description_en", "name_fi", "description_fi")],
    row.names = seq_len(nrow(source)),
    names = c(
      "source_id", "english_name", "english_description", "finnish_name",
      "finnish_description"
    )
 )
}

md_taxon_ranks <- function() {
  structure(
    taxon_rank, row.names = seq_len(nrow(taxon_rank)), names = c("rank_name")
  )
}
