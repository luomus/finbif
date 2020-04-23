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
    "admin_status",
    "red_list",
    "countries",
    "provinces",
    "municipalities",
    "bird_assoc_areas",
    "finnish_occurrence_status",
    "habitat_types",
    "habitat_qualifiers",
    "life_stages",
    "record_basis",
    "restriction_levels",
    "restriction_reasons",
    "sex_categories",
    "sources",
    "taxon_ranks"
  )

  ans <-
    if (missing(which)) data.frame(metadata_name)
    else {

      if (!which %in% metadata_name) {
        stop(which, " not found in FinBIF metadata.")
      }

      switch(
        which,
        admin_status              = md_admin_status(),
        red_list                  = md_red_list(),
        countries                 = md_countries(),
        provinces                 = md_provinces(),
        municipalities            = md_municipalities(),
        bird_assoc_areas          = md_bird_assoc_areas(),
        finnish_occurrence_status = md_finnish_occurrence_status(),
        habitat_types             = md_habitat_types(),
        habitat_qualifiers        = md_habitat_qualifiers(),
        life_stages               = md_life_stages(),
        record_basis              = md_record_basis(),
        restriction_levels        = md_restriction_levels(),
        restriction_reasons       = md_restriction_reasons(),
        sex_categories            = md_sex_categories(),
        sources                   = md_sources(),
        taxon_ranks               = md_taxon_ranks()
      )
    }

  class(ans) <- c("finbif_metadata_df", "data.frame")

  ans

}

md_admin_status <- function() {
  df <- administrative_status
  df <- df[order(df[["translated_status"]]), ]
  structure(
    df, row.names = seq_len(nrow(df)), names = c("status_name", "status_code")
  )
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
  structure(
    df, row.names = seq_len(nrow(df)), names = c("habitat_name", "habitat_code")
  )
}

md_habitat_qualifiers <- function() {
  df <- primary_habitat[["specific_habitat_types"]]
  structure(
    df, row.names = seq_len(nrow(df)),
    names = c("qualifier_name", "qualifier_code")
  )
}

md_life_stages <- function() {
  structure(life_stage, row.names = seq_len(nrow(life_stage)))
}

md_record_basis <- function() {
  structure(
    record_basis[c("description", "name")],
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
