#' FinBIF metadata
#'
#' Display metadata from the FinBIF database.
#'
#' @param which Character. Which category of metadata to display. If unspecified
#'   function returns the categories of metadata available.
#'
#' @return A data.frame.
#' @examples
#' finbif_metadata("red_list")
#' @export
finbif_metadata <- function(which) {

  metadata_types <- c(
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

  if (missing(which)) return(data.frame(type = metadata_types))

  if (!which %in% metadata_types) stop(which, " not found in FinBIF metadata.")

  switch(
    which,
    admin_status = md_admin_status(),
    red_list = md_red_list(),
    countries = md_countries(),
    provinces = md_provinces(),
    municipalities = md_municipalities(),
    bird_assoc_areas = md_bird_assoc_areas(),
    finnish_occurrence_status = md_finnish_occurrence_status(),
    habitat_types = md_habitat_types(),
    habitat_qualifiers = md_habitat_qualifiers(),
    life_stages = md_life_stages(),
    record_basis = md_record_basis(),
    restriction_levels = md_restriction_levels(),
    restriction_reasons = md_restriction_reasons(),
    sex_categories = md_sex_categories(),
    sources = md_sources(),
    taxon_ranks = md_taxon_ranks()
  )

}

md_admin_status <- function() {
  df <- administrative_status
  df <- df[order(df[["translated_status"]]), ]
  structure(df, row.names = seq_len(nrow(df)), names = c("status", "code"))
}

md_red_list <- function() {
  df <- red_list_status
  df <- df[order(df[["translated_status"]]), ]
  structure(df, row.names = seq_len(nrow(df)), names = c("status", "code"))
}

md_countries <- function() {
  structure(
    country, row.names = seq_len(nrow(country)),
    names = c("english_name", "finnish_name", "alpha_2", "alpha_3")
  )
}

md_provinces <- function() {
  structure(
    province, row.names = seq_len(nrow(province)),
    names = c("english_name", "finnish_name", "alpha", "country")
  )
}

md_municipalities <- function() {
  structure(
    municipality, row.names = seq_len(nrow(municipality)),
    names = c("english_name", "finnish_name", "country")
  )
}

md_bird_assoc_areas <- function() {
  structure(
    bird_assoc_area, row.names = seq_len(nrow(bird_assoc_area)),
    names = c("name", "code")
  )
}

md_finnish_occurrence_status <- function() {
  structure(
    finnish_occurrence_status,
    row.names = seq_len(nrow(finnish_occurrence_status)),
    names = c("description", "code")
  )
}

md_habitat_types <- function() {
  df <- primary_habitat[["habitat_types"]]
  structure(df, row.names = seq_len(nrow(df)))
}

md_habitat_qualifiers <- function() {
  df <- primary_habitat[["specific_habitat_types"]]
  structure(df, row.names = seq_len(nrow(df)))
}

md_life_stages <-
  function() structure(life_stage, row.names = seq_len(nrow(life_stage)))

md_record_basis <- function() {
  structure(
    record_basis[c("name", "description")],
    row.names = seq_len(nrow(record_basis))
  )
}

md_restriction_reasons <- function() {
  structure(
    restriction_reason,
    row.names = seq_len(nrow(restriction_reason)),
    names = c("reason", "description")
  )
}

md_restriction_levels <- function() {
  structure(
    restriction_level,
    row.names = seq_len(nrow(restriction_level)),
    names = c("level", "description")
  )
}

md_sex_categories <- function() {
  sex <- sex[order(sex[["category"]]), ]
  structure(sex, row.names = seq_len(nrow(sex)))
}

md_sources <-
  function() structure(source, row.names = seq_len(nrow(source)))

md_taxon_ranks <-
  function() structure(taxon_rank, row.names = seq_len(nrow(taxon_rank)))
