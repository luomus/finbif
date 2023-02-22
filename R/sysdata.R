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

    sd_i <- sub("^.* – ", "", sd_i)

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

get_code <- function(obj) {

  prefix <- obj[["prefix"]]

  suffix <- obj[["suffix"]]

  x <- paste0(prefix, suffix)

  ht <- get_sysdata(x)

  ht_row_names <- row.names(ht)

  ht_code <- sub(prefix, "", ht_row_names)

  ht_code <- structure(ht_code, class = "translation")

  ht_code <- list(code = ht_code)

  ht_code <- structure(ht_code, class = "data.frame", row.names = ht_row_names)

  cbind(ht_code, ht)

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

  status_code <- c(
    MX.finlex160_1997_appendix4 = "FNLX160_97_4",
    MX.finlex160_1997_appendix4_specialInterest = "FNLX160_97_4_SI",
    MX.finlex160_1997_appendix2a = "FNLX160_97_2A",
    MX.finlex160_1997_appendix2b = "FNLX160_97_2B",
    MX.finlex160_1997_appendix3a = "FNLX160_97_3A",
    MX.finlex160_1997_appendix3b = "FNLX160_97_3B",
    MX.finlex160_1997_appendix3c = "FNLX160_97_3C",
    MX.habitatsDirectiveAnnexII = "HABDIR2",
    MX.habitatsDirectiveAnnexIV = "HABDIR4",
    MX.habitatsDirectiveAnnexV = "HABDIR5",
    MX.primaryInterestInEU = "PI_EU",
    MX.habitatsDirectiveAnnexIIExceptionGranted = "HABDIR2E",
    MX.habitatsDirectiveAnnexIVExceptionGranted = "HABDIR4E",
    MX.habitatsDirectiveAnnexVExceptionGranted = "HABDIR5E",
    MX.birdsDirectiveStatusAppendix1 = "BDS1",
    MX.birdsDirectiveStatusAppendix2A = "BDS2A",
    MX.birdsDirectiveStatusAppendix2B = "BDS2B",
    MX.birdsDirectiveStatusAppendix3A = "BDS3A",
    MX.birdsDirectiveStatusAppendix3B = "BDS3B",
    MX.finnishEnvironmentInstitute2010protectionPrioritySpecies = "FEI_PPS",
    MX.finnishEnvironmentInstitute2020conservationProjectSpecies = "FEI_CPS",
    MX.finnishEnvironmentInstitute2020conservationProjectAapamireSpecies =
      "FEI_CPAS",
    MX.gameBird = "GMEB",
    MX.gameMammal = "GMEM",
    MX.unprotectedSpecies = "UNP_SP",
    MX.nationallySignificantInvasiveSpecies = "NS_INVSV",
    MX.euInvasiveSpeciesList = "EU_INVSV",
    MX.otherPlantPest = "OPP_INVSV",
    MX.qualityPlantPest = "QLPP_INVSV",
    MX.quarantinePlantPest = "QPP_INVSV",
    MX.nationalInvasiveSpeciesStrategy = "NSS_INVSV",
    MX.otherInvasiveSpeciesList = "OTH_INVSV",
    MX.controllingRisksOfInvasiveAlienSpecies = "CRAS_INVSV",
    MX.finnishEnvironmentInstitute20072010forestSpecies = "FEI_FS",
    MX.finnishEnvironmentInstitute20192021forestSpecies = "FEI_FS2",
    MX.cropWildRelative = "CWR",
    MX.birdsDirectiveStatusMigratoryBirds = "BDSMB",
    MX.cites_appendixI = "CITES1",
    MX.cites_appendixII = "CITES2",
    MX.cites_appendixIII = "CITES3",
    MX.euRegulation_cites_appendixA = "EU_CITESA",
    MX.euRegulation_cites_appendixB = "EU_CITESB",
    MX.finlex160_1997_appendix4_2021 = "FNLX160_97_4_2021",
    MX.finlex160_1997_appendix4_specialInterest_2021 = "FNLX160_97_4_SI_2021",
    MX.finlex160_1997_largeBirdsOfPrey = "FNLX160_97_LBP",
    MX.finlex160_1997_appendix1 = "FNLX160_97_1",
    MX.finnishEnvironmentInstitute2020protectionPrioritySpecies = "FEI2020PPS",
    MX.finnishEnvironmentInstitute2020conservationProjectVascularSpecies =
      "FEI2020CPVPS",
    MX.habitatsDirectiveAnnexII_FinlandNaturaSpecies = "HABDIR2FN",
    MX.euRegulation_cites_appendixD = "EU_CITESD"
  )

  reg_status <- get_sysdata("MX.adminStatusEnum")

  id <- row.names(reg_status)

  status_code <- status_code[id]

  status_code_na <- is.na(status_code)

  missing_status_codes <- id[status_code_na]

  missing_status_codes <- sub("^.*\\.", "", missing_status_codes)

  missing_status_codes <- abbreviate(missing_status_codes, 20L)

  missing_status_codes <- toupper(missing_status_codes)

  status_code[status_code_na] <- missing_status_codes

  status_code <- make.unique(status_code)

  status_code <- structure(status_code, class = "translation")

  status_code <- list(status_code = status_code)

  status_code <- structure(status_code, class = "data.frame", row.names = id)

  cbind(status_code, reg_status)

}

#' @noRd

red_list_status <- function() {

  red_list <- list(prefix = "MX.iucn", suffix = "Statuses")

  get_code(red_list)

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

  habitat_types <- list(prefix = "MKV.habitat", suffix = "Enum")

  habitat_types <- get_code(habitat_types)

  specific_types <-  list(prefix = "MKV.habitatSpecificType", suffix = "Enum")

  specific_types <- get_code(specific_types)

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

  get_sysdata("MY.atlasCodeEnum")

}

#' @noRd

atlas_class <- function() {

  get_sysdata("MY.atlasClassEnum")

}

#' @noRd

abundance_unit <- function() {

  get_sysdata("MY.abundanceUnitEnum")

}
