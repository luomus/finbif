#' @noRd

get_sysdata <- function(x) {

  path <- paste0("metadata/ranges/", x)

  query <- list(lang = "multi")

  request <- list(path = path, query = query, cache = TRUE)

  sd_response <- api_get(request)

  sd_response_content <- sd_response[["content"]]

  n_langs <- length(supported_langs)

  sq <- seq_len(n_langs)

  sd_df <- vector("list", n_langs)

  row_names <- vapply(sd_response_content, getElement, "", "id")

  col_names <- paste0("name_", supported_langs)

  sd_df <- structure(sd_df, row.names = row_names, names = col_names)

  sq <- seq_along(supported_langs)

  for (i in sq) {

    lang_i <- supported_langs[[i]]

    el_i <- c("value", lang_i)

    sd_i <- vapply(sd_response_content, get_el_recurse, "", el_i, "character")

    sd_df[[i]] <- sd_i

  }

  sd_df <- set_translations(sd_df)

  structure(sd_df, class = "data.frame")

}

#' @noRd

get_enumeration <- function(x) {

  path <- "warehouse/enumeration-labels"

  query <- list()

  request <- list(path = path, query = query, cache = TRUE)

  en_response <- api_get(request)

  results <- c("content", "results")

  en_response_results <- en_response[[results]]

  enumerations <- vapply(en_response_results, getElement, "", "enumeration")

  ids <- vapply(en_response_results, getElement, "", "property")

  enumerations <- structure(enumerations, names = ids)

  sd_en_df <- get_sysdata(x)

  id <- row.names(sd_en_df)

  id <- enumerations[id]

  non_missing_enums <- !is.na(id)

  sd_en_df <- structure(sd_en_df, row.names = id)

  sd_en_df <- sd_en_df[non_missing_enums, ]

  id <- id[non_missing_enums]

  enumeration <- tolower(id)

  enumeration <- list(enumeration = enumeration)

  enumeration <- structure(enumeration, class = "data.frame", row.names = id)

  sd_en_df <- cbind(enumeration, sd_en_df)

  set_translations(sd_en_df)

}

#' @noRd

set_translations <- function(x) {

  x[] <- lapply(x, structure, class = "translation")

  x

}

#' @noRd

var_names <- function() {

  var_names_df

}

#' @noRd

has_value <- function() {

  has_value_df

 }

#' @noRd

cite_file_vars <- function() {

  cite_file_vars_df

}

#' @noRd

lite_download_file_vars <- function() {

  lite_download_file_vars_df

}

#' @noRd

filter_names <- function() {

  filter_names_df

}

#' @noRd

regulatory_status <- function() {

  regulatory_status_df

}

#' @noRd

red_list_status <- function() {

  red_list_status_df

}

#' @noRd

threatened_status <- function() {

  get_sysdata("MX.threatenedStatusEnum")

}

#' @noRd

informal_groups <- function() {

  informal_groups_df

}

#' @noRd

informal_groups_reported <- informal_groups

#' @noRd

get_habitat_types <- function(x) {

  enum <- paste0(x, "Enum")

  ht <- get_sysdata(enum)

  ht_row_names <- row.names(ht)

  ht_code <- sub(x, "", ht_row_names)

  ht_code <- structure(ht_code, class = "translation")

  ht_code <- list(code = ht_code)

  ht_code <- structure(ht_code, class = "data.frame", row.names = ht_row_names)

  cbind(ht_code, ht)

}

#' @noRd

primary_habitat <- function() {

  habitat_types <- get_habitat_types("MKV.habitat")

  specific_types <- get_habitat_types("MKV.habitatSpecificType")

  list(habitat_types = habitat_types, specific_habitat_types = specific_types)

}

#' @noRd

primary_secondary_habitat <- primary_habitat

#' @noRd

taxon_rank <- function() {

  get_sysdata("MX.threatenedStatusEnum")

}

#' @noRd

orig_taxon_rank <- taxon_rank

#' @noRd

country <- function() {

  country_df

}

#' @noRd

region <- function() {

  region_df

}

#' @noRd

bio_province <- function() {

  bio_province_df

}

#' @noRd

municipality <- function() {

  municipality_df

}

#' @noRd

bird_assoc_area <- function() {

  bird_assoc_area_df

}

#' @noRd

finnish_occurrence_status <- function() {

  finnish_occurrence_status_df

}

#' @noRd

finnish_occurrence_status_neg <- function() {

  finnish_occurrence_status_df

}

#' @noRd

source <- function() {

  source_df

}

#' @noRd

record_basis <- function() {

  get_enumeration("MY.recordBases")

}

#' @noRd

superrecord_basis <- function() {

  superrecord_basis_df

}

#' @noRd

life_stage <- function() {

  animal_life_stages <- get_enumeration("MY.lifeStages")

  plant_life_stages <- get_enumeration("MY.plantLifeStageEnum")

  life_stages <- rbind(animal_life_stages, plant_life_stages)

  set_translations(life_stages)

}

#' @noRd

sex <- function() {

  sex_df

}

#' @noRd

restriction_reason <- function() {

  get_enumeration("MZ.secureReason")

}

#' @noRd

restriction_level <- function() {

  get_enumeration("MX.secureLevels")

}

#' @noRd

quality_issues <- function() {

  quality_issues_df

}

#' @noRd

collection_quality <- function() {

  get_enumeration("MY.collectionQualityEnum")

}

#' @noRd

record_quality <- function() {

  get_enumeration("MZ.recordQualityEnum")

}

#' @noRd

record_reliability <- function() {

  record_reliability_df

}

#' @noRd

complete_list_type <- function() {

  complete_list_type_df

}

#' @noRd

location_tag <- function() {

  location_tag_df

}

#' @noRd

atlas_code <- function() {

  atlas_code_df

}

#' @noRd

atlas_class <- function() {

  atlas_class_df

}

#' @noRd

abundance_unit <- function() {

  abundance_unit_df

}
