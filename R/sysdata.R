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

    sd_i <- structure(sd_i, class = "translation")

    sd_df[[i]] <- sd_i

  }

  structure(sd_df, class = "data.frame")


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

primary_habitat <- function() {

  ht <- get_sysdata("MKV.habitatEnum")

  ht_row_names <- row.names(ht)

  ht_code <- sub("MKV.habitat", "", ht_row_names)

  ht_code <- structure(ht_code, class = "translation")

  ht_code <- list(code = ht_code)

  ht_code <- structure(ht_code, class = "data.frame", row.names = ht_row_names)

  ht <- cbind(ht_code, ht)

  st <- get_sysdata("MKV.habitatSpecificTypeEnum")

  st_row_names <- row.names(st)

  st_code <- sub("MKV.habitatSpecificType", "", st_row_names)

  st_code <- structure(st_code, class = "translation")

  st_code <- list(code = st_code)

  st_code <- structure(st_code, class = "data.frame", row.names = st_row_names)

  st <- cbind(st_code, st)

  list(habitat_types = ht, specific_habitat_types = st)

}

#' @noRd

primary_secondary_habitat <- primary_habitat

#' @noRd

taxon_rank <- function() {

  taxon_rank_df

}

#' @noRd

orig_taxon_rank <- function() {

  orig_taxon_rank_df

}

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

  record_basis_df

}

#' @noRd

superrecord_basis <- function() {

  superrecord_basis_df

}

#' @noRd

life_stage <- function() {

  life_stage_df

}

#' @noRd

sex <- function() {

  sex_df

}

#' @noRd

restriction_reason <- function() {

  restriction_reason_df

}

#' @noRd

restriction_level <- function() {

  restriction_level_df

}

#' @noRd

quality_issues <- function() {

  quality_issues_df

}

#' @noRd

collection_quality <- function() {

  collection_quality_df

}

#' @noRd

record_quality <- function() {

  record_quality_df

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
