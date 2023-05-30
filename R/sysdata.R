#' @noRd

sysdata <- function(which) {

  switch(
    which,
    supported_langs = c(English = "en", Finnish = "fi", Swedish = "sv"),
    var_names = var_names_df,
    has_value = has_value_df,
    cite_file_vars = cite_file_vars_df,
    lite_download_file_vars = lite_download_file_vars_df,
    filter_names = filter_names_df,
    regulatory_status = regulatory_status(),
    red_list_status = red_list_status(),
    threatened_status = get_sysdata("MX.threatenedStatusEnum"),
    informal_groups = informal_groups(),
    informal_groups_reported = informal_groups(),
    primary_habitat = primary_habitat(),
    primary_secondary_habitat = primary_habitat(),
    taxon_rank = get_sysdata("MX.taxonRankEnum"),
    orig_taxon_rank = get_sysdata("MX.taxonRankEnum"),
    country = get_areas("country"),
    region = get_areas("province"),
    bio_province = get_areas("biogeographicalProvince"),
    municipality = municipality(),
    bird_assoc_area = bird_assoc_area(),
    finnish_occurrence_status = finnish_occurrence_status(),
    finnish_occurrence_status_neg = finnish_occurrence_status(),
    source = sources(),
    record_basis = record_basis(),
    superrecord_basis = superrecord_basis(),
    life_stage = life_stage(),
    sex = sex(),
    restriction_reason = restriction_reason(),
    restriction_level = restriction_level(),
    quality_issues = quality_issues(),
    collection_quality = collection_quality(),
    record_quality = record_quality(),
    record_reliability = record_reliability(),
    complete_list_type = complete_list_type(),
    location_tag = get_sysdata("MNP.tagEnum"),
    atlas_code = get_sysdata("MY.atlasCodeEnum"),
    atlas_class = get_sysdata("MY.atlasClassEnum"),
    abundance_unit = get_sysdata("MY.abundanceUnitEnum")
  )

}

#' @noRd

get_sysdata <- function(x) {

  cache <- getOption("finbif_use_cache")

  cache <- cache || getOption("finbif_use_cache_metadata")

  request <- list(
    path = paste0("metadata/ranges/", x),
    query = list(lang = "multi"),
    cache = cache
  )

  sd_response <- api_get(request)

  supported_langs <- sysdata("supported_langs")

  n_langs <- length(supported_langs)

  sd_response_content <- sd_response[["content"]]

  sd_df <- structure(
    vector("list", n_langs),
    row.names =  vapply(sd_response_content, getElement, "", "id"),
    names = paste0("name_", supported_langs)
  )

  for (i in seq_len(n_langs)) {

    el_i <- c("value", supported_langs[[i]])

    sd_i <- vapply(sd_response_content, get_el_recurse, "", el_i, "character")

    sd_df[[i]] <- sub("^.* [\u2013|-] ", "", sd_i)

  }

  structure(set_translations(sd_df), class = "data.frame")

}

#' @noRd

get_enumeration <- function(obj) {

  cache <- getOption("finbif_use_cache")

  cache <- cache || getOption("finbif_use_cache_metadata")

  request <- list(
    path = "warehouse/enumeration-labels",
    cache = cache
  )

  en_response <- api_get(request)

  en_response_results <- en_response[[c("content", "results")]]

  enumerations <- vapply(en_response_results, getElement, "", "enumeration")

  ids <- vapply(en_response_results, getElement, "", "property")

  enumerations <- structure(enumerations, names = ids)

  get_function <- obj[["fun"]]

  sd_en_df <- get_function(obj[["enum"]])

  id <- enumerations[row.names(sd_en_df)]

  non_missing_enums <- !is.na(id)

  sd_en_df <- structure(sd_en_df, row.names = id)

  sd_en_df <- sd_en_df[non_missing_enums, ]

  id <- id[non_missing_enums]

  enumeration <- list(code = tolower(id))

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

  x <- paste0(prefix, obj[["suffix"]])

  ht <- get_sysdata(x)

  ht_row_names <- row.names(ht)

  ht_code <- sub(prefix, "", ht_row_names)

  ht_code <- structure(ht_code, class = "translation")

  ht_code <- list(code = ht_code)

  ht_code <- structure(ht_code, class = "data.frame", row.names = ht_row_names)

  cbind(ht_code, ht)

}

#' @noRd

get_areas <- function(x) {

  cache <- getOption("finbif_use_cache")

  cache <- cache || getOption("finbif_use_cache_metadata")

  request <- list(
    path = "areas",
    query = list(type = x, lang = "multi", pageSize = 1000L),
    cache = cache
  )

  sd_response <- api_get(request)

  sd_response_results <- sd_response[[c("content", "results")]]

  supported_langs <- sysdata("supported_langs")

  col_names <- paste0("name_", supported_langs)

  n_cols <- length(col_names)

  sd_df <- structure(
    vector("list", n_cols),
    row.names = vapply(sd_response_results, getElement, "", "id"),
    names = col_names
  )

  for (i in seq_along(supported_langs)) {

    el_i <- c("name", supported_langs[[i]])

    sd_i <- vapply(sd_response_results, get_el_recurse, "", el_i, "character")

    sd_df[[i]] <- sub("^.* [\u2013|-] ", "", sd_i)

  }

  c_els <- paste0("countryCodeISO", "alpha2")

  ccode <- vapply(sd_response_results, get_el_recurse, "", c_els, "character")

  if (!all_na(ccode)) {

    sd_df[["code"]] <- ccode

  }

  p_els <- c("provinceCodeAlpha", "fi")

  pcode <- vapply(sd_response_results, get_el_recurse, "", p_els, "character")

  if (!all_na(pcode)) {

    sd_df[["code"]] <- pcode

  }

  sd_df <- replace_missing_names(sd_df)

  structure(set_translations(sd_df), class = "data.frame")

}

#' @noRd

replace_missing_names <- function(df) {

  supported_langs <- sysdata("supported_langs")

  col_names <- paste0("name_", supported_langs)

  for (i in col_names) {

    col <- df[[i]]

    for (j in setdiff(col_names, i)) {

      missing <- is.na(col)

      other_col <- df[[j]]

      col[missing] <- other_col[missing]

    }

    df[[i]] <- col

  }

  df

}

#' @noRd

regulatory_status <- function() {

  code <- c(
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

  code <- code[id]

  code_na <- is.na(code)

  missing_codes <- id[code_na]

  missing_codes <- sub("^.*\\.", "", missing_codes)

  missing_codes <- abbreviate(missing_codes, 20L)

  code[code_na] <- toupper(missing_codes)

  code <- make.unique(code)

  code <- structure(code, class = "translation")

  code <- list(code = code)

  code <- structure(code, class = "data.frame", row.names = id)

  cbind(code, reg_status)

}

#' @noRd

red_list_status <- function() {

  red_list <- list(prefix = "MX.iucn", suffix = "Statuses")

  get_code(red_list)

}

#' @noRd

informal_groups <- function() {

  cache <- getOption("finbif_use_cache")

  cache <- cache || getOption("finbif_use_cache_metadata")

  request <- list(
    path = "informal-taxon-groups",
    query = list(lang = "multi", pageSize = 1000L),
    cache = cache
  )

  sd_response <- api_get(request)

  sd_response_content <- sd_response[[ c("content", "results")]]

  supported_langs <- sysdata("supported_langs")

  n_langs <- length(supported_langs)

  sd_df <- structure(
    vector("list", n_langs),
    row.names = vapply(sd_response_content, getElement, "", "id"),
    names = paste0("name_", supported_langs)
  )

  for (i in seq_len(n_langs)) {

    el_i <- c("name", supported_langs[[i]])

    sd_i <- vapply(sd_response_content, get_el_recurse, "", el_i, "character")

    sd_df[[i]] <- sub("^.* [\u2013|-] ", "", sd_i)

  }

  structure(set_translations(sd_df), class = "data.frame")

}

#' @noRd

primary_habitat <- function() {

  habitat_types <- list(prefix = "MKV.habitat", suffix = "Enum")

  specific_types <-  list(prefix = "MKV.habitatSpecificType", suffix = "Enum")

  list(
    habitat_types = get_code(habitat_types),
    specific_habitat_types = get_code(specific_types)
  )

}

#' @noRd

municipality <- function() {

  regions <- c(
    ML.351 = "Keski-Suomi",
    ML.352 = "Etel\u00e4-Pohjanmaa",
    ML.353 = "Pirkanmaa",
    ML.354 = "Etel\u00e4-Pohjanmaa",
    ML.355 = "Pohjois-Pohjanmaa",
    ML.356 = "Etel\u00e4-Pohjanmaa",
    ML.357 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.358 = "Uusimaa",
    ML.359 = "Varsinais-Suomi",
    ML.360 = "Ahvenanmaa",
    ML.361 = "Ahvenanmaa",
    ML.362 = "Etel\u00e4-Savo",
    ML.363 = "Lappi",
    ML.364 = "Uusimaa",
    ML.365 = "Satakunta",
    ML.366 = "Satakunta",
    ML.367 = "Etel\u00e4-Pohjanmaa",
    ML.368 = "Ahvenanmaa",
    ML.369 = "Ahvenanmaa",
    ML.370 = "Kanta-H\u00e4me",
    ML.371 = "Ahvenanmaa",
    ML.372 = "Pohjois-Pohjanmaa",
    ML.373 = "Pohjois-Pohjanmaa",
    ML.374 = "Pohjois-Pohjanmaa",
    ML.375 = "Keski-Pohjanmaa",
    ML.376 = "Pirkanmaa",
    ML.377 = "Kanta-H\u00e4me",
    ML.378 = "Kymenlaakso",
    ML.379 = "Ahvenanmaa",
    ML.380 = "Keski-Suomi",
    ML.381 = "Uusimaa",
    ML.382 = "Satakunta",
    ML.383 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.384 = "Kanta-H\u00e4me",
    ML.385 = "Kanta-H\u00e4me",
    ML.386 = "Pohjois-Karjala",
    ML.387 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.388 = "Uusimaa",
    ML.389 = "Etel\u00e4-Savo",
    ML.390 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.391 = "Satakunta",
    ML.392 = "Kanta-H\u00e4me",
    ML.393 = "Kainuu",
    ML.394 = "Uusimaa",
    ML.396 = "Pohjois-Pohjanmaa",
    ML.397 = "Pohjois-Savo",
    ML.398 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.399 = "Pirkanmaa",
    ML.400 = "Etel\u00e4-Pohjanmaa",
    ML.401 = "Pohjois-Karjala",
    ML.402 = "Etel\u00e4-Karjala",
    ML.403 = "Lappi",
    ML.404 = "Uusimaa",
    ML.405 = "Etel\u00e4-Pohjanmaa",
    ML.406 = "Etel\u00e4-Pohjanmaa",
    ML.407 = "Satakunta",
    ML.408 = "Keski-Suomi",
    ML.409 = "Kanta-H\u00e4me",
    ML.410 = "Uusimaa",
    ML.411 = "Pohjois-Karjala",
    ML.412 = "Kanta-H\u00e4me",
    ML.413 = "Ahvenanmaa",
    ML.414 = "Pohjois-Savo",
    ML.415 = "Keski-Suomi",
    ML.416 = "Pohjois-Karjala",
    ML.417 = "Pirkanmaa",
    ML.418 = "Etel\u00e4-Savo",
    ML.419 = "Keski-Suomi",
    ML.420 = "Varsinais-Suomi",
    ML.421 = "Pohjois-Savo",
    ML.422 = "Kainuu",
    ML.423 = "Pohjois-Pohjanmaa",
    ML.424 = "Pirkanmaa",
    ML.425 = "Etel\u00e4-Savo",
    ML.426 = "Satakunta",
    ML.427 = "Keski-Suomi",
    ML.428 = "Keski-Pohjanmaa",
    ML.429 = "Etel\u00e4-Pohjanmaa",
    ML.430 = "Uusimaa",
    ML.431 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.432 = "Pohjois-Pohjanmaa",
    ML.433 = "Keski-Suomi",
    ML.434 = "Satakunta",
    ML.435 = "Pohjanmaa",
    ML.436 = "Etel\u00e4-Pohjanmaa",
    ML.437 = "Etel\u00e4-Pohjanmaa",
    ML.438 = "Uusimaa",
    ML.439 = "Keski-Pohjanmaa",
    ML.440 = "Pohjois-Savo",
    ML.442 = "Lappi",
    ML.443 = "Lappi",
    ML.444 = "Lappi",
    ML.445 = "Varsinais-Suomi",
    ML.446 = "Pohjois-Pohjanmaa",
    ML.447 = "Uusimaa",
    ML.449 = "Keski-Suomi",
    ML.450 = "Pirkanmaa",
    ML.451 = "Keski-Suomi",
    ML.452 = "Uusimaa",
    ML.453 = "Pohjois-Karjala",
    ML.454 = "Lappi",
    ML.455 = "Pohjois-Savo",
    ML.456 = "Keski-Suomi",
    ML.457 = "Ahvenanmaa",
    ML.458 = "Satakunta",
    ML.459 = "Keski-Pohjanmaa",
    ML.460 = "Lappi",
    ML.461 = "Keski-Suomi",
    ML.462 = "Pohjois-Karjala",
    ML.464 = "Pohjanmaa",
    ML.465 = "Varsinais-Suomi",
    ML.466 = "Kymenlaakso",
    ML.467 = "Kymenlaakso",
    ML.468 = "Pohjanmaa",
    ML.469 = "Pohjanmaa",
    ML.470 = "Kainuu",
    ML.471 = "Pirkanmaa",
    ML.472 = "Ahvenanmaa",
    ML.473 = "Pohjois-Savo",
    ML.474 = "Etel\u00e4-Pohjanmaa",
    ML.475 = "Etel\u00e4-Pohjanmaa",
    ML.476 = "Varsinais-Suomi",
    ML.477 = "Pohjois-Pohjanmaa",
    ML.478 = "Keski-Suomi",
    ML.479 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.480 = "Pohjanmaa",
    ML.481 = "Varsinais-Suomi",
    ML.482 = "Uusimaa",
    ML.483 = "Pohjois-Savo",
    ML.484 = "Etel\u00e4-Pohjanmaa",
    ML.485 = "Etel\u00e4-Karjala",
    ML.486 = "Etel\u00e4-Pohjanmaa",
    ML.487 = "Keski-Suomi",
    ML.488 = "Etel\u00e4-Karjala",
    ML.489 = "Ahvenanmaa",
    ML.490 = "Pirkanmaa",
    ML.491 = "Pohjois-Savo",
    ML.492 = "Keski-Pohjanmaa",
    ML.493 = "Pohjois-Karjala",
    ML.494 = "Varsinais-Suomi",
    ML.495 = "Pohjois-Pohjanmaa",
    ML.496 = "Pohjois-Karjala",
    ML.497 = "Uusimaa",
    ML.498 = "Varsinais-Suomi",
    ML.499 = "Kanta-H\u00e4me",
    ML.500 = "Uusimaa",
    ML.501 = "Keski-Suomi",
    ML.502 = "Pohjois-Pohjanmaa",
    ML.503 = "Ahvenanmaa",
    ML.504 = "Pohjanmaa",
    ML.505 = "Etel\u00e4-Karjala",
    ML.506 = "Pohjanmaa",
    ML.507 = "Ahvenanmaa",
    ML.508 = "Uusimaa",
    ML.509 = "Pirkanmaa",
    ML.510 = "Etel\u00e4-Savo",
    ML.512 = "Varsinais-Suomi",
    ML.513 = "Varsinais-Suomi",
    ML.514 = "Pohjois-Pohjanmaa",
    ML.515 = "Satakunta",
    ML.516 = "Kymenlaakso",
    ML.517 = "Etel\u00e4-Savo",
    ML.518 = "Pohjois-Pohjanmaa",
    ML.519 = "Keski-Suomi",
    ML.520 = "Lappi",
    ML.521 = "Pohjanmaa",
    ML.522 = "Keski-Suomi",
    ML.523 = "Varsinais-Suomi",
    ML.524 = "Uusimaa",
    ML.525 = "Varsinais-Suomi",
    ML.526 = "Satakunta",
    ML.527 = "Pohjanmaa",
    ML.528 = "Pohjois-Pohjanmaa",
    ML.529 = "Pirkanmaa",
    ML.530 = "Varsinais-Suomi",
    ML.531 = "Pohjois-Karjala",
    ML.532 = "Uusimaa",
    ML.533 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.534 = "Varsinais-Suomi",
    ML.536 = "Pirkanmaa",
    ML.537 = "Pohjois-Pohjanmaa",
    ML.538 = "Pohjois-Pohjanmaa",
    ML.539 = "Pohjois-Karjala",
    ML.541 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.542 = "Varsinais-Suomi",
    ML.543 = "Pirkanmaa",
    ML.544 = "Kainuu",
    ML.545 = "Varsinais-Suomi",
    ML.546 = "Etel\u00e4-Karjala",
    ML.547 = "Pirkanmaa",
    ML.548 = "Pohjanmaa",
    ML.549 = "Lappi",
    ML.550 = "Lappi",
    ML.551 = "Keski-Pohjanmaa",
    ML.552 = "Etel\u00e4-Savo",
    ML.553 = "Keski-Suomi",
    ML.554 = "Etel\u00e4-Savo",
    ML.555 = "Pohjois-Savo",
    ML.556 = "Pohjanmaa",
    ML.557 = "Keski-Suomi",
    ML.558 = "Pirkanmaa",
    ML.559 = "Pohjois-Karjala",
    ML.560 = "Satakunta",
    ML.561 = "Satakunta",
    ML.562 = "Uusimaa",
    ML.563 = "Uusimaa",
    ML.564 = "Lappi",
    ML.565 = "Varsinais-Suomi",
    ML.566 = "Pohjois-Pohjanmaa",
    ML.567 = "Uusimaa",
    ML.568 = "Pirkanmaa",
    ML.569 = "Kainuu",
    ML.570 = "Etel\u00e4-Savo",
    ML.572 = "Pohjois-Pohjanmaa",
    ML.573 = "Pohjois-Pohjanmaa",
    ML.574 = "Pohjois-Pohjanmaa",
    ML.575 = "Varsinais-Suomi",
    ML.576 = "Kymenlaakso",
    ML.577 = "Pohjois-Pohjanmaa",
    ML.578 = "Pohjois-Karjala",
    ML.579 = "Uusimaa",
    ML.580 = "Varsinais-Suomi",
    ML.582 = "Etel\u00e4-Savo",
    ML.584 = "Lappi",
    ML.585 = "Satakunta",
    ML.586 = "Pohjois-Savo",
    ML.587 = "Pohjois-Savo",
    ML.588 = "Etel\u00e4-Karjala",
    ML.589 = "Pohjois-Pohjanmaa",
    ML.590 = "Kanta-H\u00e4me",
    ML.591 = "Kainuu",
    ML.592 = "Lappi",
    ML.593 = "Etel\u00e4-Karjala",
    ML.594 = "Pirkanmaa",
    ML.595 = "Varsinais-Suomi",
    ML.596 = "Keski-Suomi",
    ML.597 = "Satakunta",
    ML.598 = "Lappi",
    ML.599 = "Varsinais-Suomi",
    ML.600 = "Ahvenanmaa",
    ML.601 = "Pirkanmaa",
    ML.602 = "Varsinais-Suomi",
    ML.603 = "Etel\u00e4-Karjala",
    ML.604 = "Etel\u00e4-Savo",
    ML.605 = "Lappi",
    ML.606 = "Etel\u00e4-Pohjanmaa",
    ML.607 = "Pohjois-Pohjanmaa",
    ML.608 = "Satakunta",
    ML.609 = "Pohjois-Pohjanmaa",
    ML.610 = "Pohjois-Pohjanmaa",
    ML.611 = "Pohjois-Savo",
    ML.612 = "Lappi",
    ML.613 = "Uusimaa",
    ML.614 = "Uusimaa",
    ML.615 = "Lappi",
    ML.616 = "Etel\u00e4-Pohjanmaa",
    ML.617 = "Varsinais-Suomi",
    ML.618 = "Pohjois-Savo",
    ML.619 = "Kainuu",
    ML.620 = "Ahvenanmaa",
    ML.621 = "Etel\u00e4-Savo",
    ML.622 = "Ahvenanmaa",
    ML.623 = "Kainuu",
    ML.624 = "Pohjois-Savo",
    ML.625 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.626 = "Etel\u00e4-Karjala",
    ML.627 = "Pohjois-Pohjanmaa",
    ML.628 = "Varsinais-Suomi",
    ML.629 = "Kanta-H\u00e4me",
    ML.630 = "Pirkanmaa",
    ML.631 = "Pohjois-Savo",
    ML.632 = "Lappi",
    ML.633 = "Etel\u00e4-Pohjanmaa",
    ML.634 = "Pohjois-Karjala",
    ML.635 = "Keski-Pohjanmaa",
    ML.636 = "Keski-Suomi",
    ML.637 = "Lappi",
    ML.638 = "Varsinais-Suomi",
    ML.639 = "Pohjois-Savo",
    ML.640 = "Uusimaa",
    ML.641 = "Pohjois-Pohjanmaa",
    ML.642 = "Satakunta",
    ML.643 = "Pirkanmaa",
    ML.644 = "Pohjois-Pohjanmaa",
    ML.645 = "Lappi",
    ML.647 = "Keski-Suomi",
    ML.648 = "Pohjanmaa",
    ML.649 = "Varsinais-Suomi",
    ML.650 = "Pohjois-Pohjanmaa",
    ML.651 = "Pohjanmaa",
    ML.652 = "Pirkanmaa",
    ML.653 = "Uusimaa",
    ML.654 = "Ahvenanmaa",
    ML.655 = "Pohjois-Savo",
    ML.656 = "Varsinais-Suomi",
    ML.657 = "Pohjois-Savo",
    ML.658 = "Pirkanmaa",
    ML.659 = "Keski-Pohjanmaa",
    ML.660 = "Pohjois-Savo",
    ML.661 = "Uusimaa",
    ML.662 = "Keski-Suomi",
    ML.663 = "Etel\u00e4-Pohjanmaa",
    ML.664 = "Kymenlaakso",
    ML.665 = "Pirkanmaa",
    ML.666 = "Pohjanmaa",
    ML.667 = "Lappi",
    ML.668 = "Pohjois-Pohjanmaa",
    ML.669 = "Pirkanmaa",
    ML.670 = "Kanta-H\u00e4me"
  )

  municipalities <- get_areas("municipality")

  id <- row.names(municipalities)

  regions <- structure(regions[id], class = "translation", names = NULL)

  regions <- list(region = regions)

  regions <- structure(regions, class = "data.frame", row.names = id)

  cbind(municipalities, regions)

}

#' @noRd

bird_assoc_area <- function() {

  codes <- c(
    ML.1088 = "AFF",
    ML.1095 = "EKLY",
    ML.1101 = "Oriolus",
    ML.1091 = "Tringa",
    ML.1110 = "KLY",
    ML.1097 = "KHLY",
    ML.1111 = "Xenus",
    ML.1092 = "Apus",
    ML.1108 = "KPLY",
    ML.1104 = "KSLY",
    ML.1113 = "Kuusamo",
    ML.1094 = "KyLY",
    ML.1112 = "LLY",
    ML.1114 = "Hakki",
    ML.1096 = "LHLH",
    ML.1107 = "MLY",
    ML.1098 = "PHLY",
    ML.1099 = "PiLY",
    ML.1103 = "PKLY",
    ML.1109 = "PPLY",
    ML.1102 = "Kuikka",
    ML.1090 = "PLY",
    ML.1093 = "PSLY",
    ML.1116 = "RSLH",
    ML.1127 = "SUA",
    ML.1105 = "SSLTY",
    ML.1106 = "SpLY",
    ML.1089 = "TLY",
    ML.1267 = "OA"
  )

  bird_assoc <- get_areas("birdAssociationArea")

  id <- row.names(bird_assoc)

  codes <- unname(codes[id])

  codes <- structure(codes, class = "translation", names = NULL)

  codes <- list(code = codes)

  codes <- structure(codes, class = "data.frame", row.names = id)

  cbind(bird_assoc, codes)

}

#' @noRd

finnish_occurrence_status <- function() {

  codes <- c(
    MX.doesNotOccur = "none",
    MX.typeOfOccurrenceOccurs = "occurs",
    MX.typeOfOccurrenceStablePopulation = "stable",
    MX.typeOfOccurrenceCommon = "common",
    MX.typeOfOccurrenceRare = "rare",
    MX.typeOfOccurrenceVeryRare = "very_rare",
    MX.typeOfOccurrenceVagrant = "vagrant_regular",
    MX.typeOfOccurrenceRareVagrant = "vagrant_irregular",
    MX.typeOfOccurrenceMigrant = "migrant",
    MX.typeOfOccurrenceImport = "import",
    MX.typeOfOccurrenceAnthropogenic = "introduced",
    MX.typeOfOccurrenceNotEstablished = "unestablished",
    MX.typeOfOccurrenceExtirpated = "extinct",
    MX.typeOfOccurrenceOldRecords = "historic",
    MX.typeOfOccurrenceUncertain = "uncertain",
    MX.typeOfOccurrenceMaxReplanted = "reintroduced",
    MX.typeOfOccurrenceNotEvaluated = "unevaluated",
    MX.typeOfOccurrenceSpontaneousOldResident = "SOR",
    MX.typeOfOccurrenceSpontaneousOldFormerlyResidentPossiblyExtinct = "SORPE",
    MX.typeOfOccurrenceSpontaneousOldFormerlyResidentExtinct = "SORE",
    MX.typeOfOccurrenceSpontaneousNewResident = "SNR",
    MX.typeOfOccurrenceSpontaneousNewEphemeral = "SNE",
    MX.typeOfOccurrenceSpontaneousNewEphemeralOnlyOld = "SNEH",
    MX.typeOfOccurrenceAlienOldResident = "AOR",
    MX.typeOfOccurrenceAlienOldFormerlyResidentPossiblyExtinct = "AORPE",
    MX.typeOfOccurrenceAlienOldExtinct = "AORE",
    MX.typeOfOccurrenceAlienNewResident = "ANR",
    MX.typeOfOccurrenceAlienNewEphemeral = "ANE",
    MX.typeOfOccurrenceAlienNewEphemeralOnlyold = "ANEH",
    MX.typeOfOccurrenceCompletelyCultivatedOrigin = "cultivation_escape_all",
    MX.typeOfOccurrenceNotableDegreeCultivatedOrigin = "cultivation_escape",
    MX.typeOfOccurrenceMaxShortDistanceEscape = "cultivation_escape_short",
    MX.typeOfOccurrenceSmallDegreeCultivatedOrigin = "cultivation_escape_some",
    MX.typeOfOccurrenceOnlyCultivated = "cultivation_only",
    MX.typeOfOccurrenceMaxRelict = "cultivation_relict",
    MX.typeOfOccurrenceMaxSoilImmigrant = "soil_immigrant",
    MX.typeOfOccurrenceOccursBasedOnOccurrences = "records_only",
    MX.typeOfOccurrenceRegularBreeder = "regular_breeder",
    MX.typeOfOccurrenceIrregularBreeder = "irregular_breeder",
    MX.typeOfOccurrencePassageMigrant = "passage_migrant",
    MX.typeOfOccurrenceBirdLifeCategoryA = "BLA",
    MX.typeOfOccurrenceBirdLifeCategoryB = "BLB",
    MX.typeOfOccurrenceBirdLifeCategoryC = "BLC",
    MX.typeOfOccurrenceBirdLifeCategoryD = "BLD",
    MX.typeOfOccurrenceBirdLifeCategoryE = "BLE"
  )

  finnish_occurrence <- get_sysdata("MX.typeOfOccurrenceEnum")

  id <- row.names(finnish_occurrence)

  codes <- unname(codes[id])

  codes <- structure(codes, class = "translation", names = NULL)

  codes <- list(code = codes)

  codes <- structure(codes, class = "data.frame", row.names = id)

  cbind(finnish_occurrence, codes)

}

#' @noRd

sources <- function() {

  cache <- getOption("finbif_use_cache")

  cache <- cache || getOption("finbif_use_cache_metadata")

  request <- list(
    path = "sources",
    query = list(lang = "multi", pageSize = 1000L),
    cache = cache
  )

  sd_response <- api_get(request)

  sd_response_results <- sd_response[[c("content", "results")]]

  row_names <- vapply(sd_response_results, getElement, "", "id")

  types <- c("name", "description")

  supported_langs <- sysdata("supported_langs")

  n_langs <- length(supported_langs)

  col_names <- rep(types, each = n_langs)

  col_names <- paste(col_names, supported_langs, sep = "_")

  n_cols <- length(col_names)

  sd_df <- vector("list", n_cols)

  sd_df <- structure(sd_df, row.names = row_names, names = col_names)

  sd_df[["code"]] <- row_names

  sq <- seq_along(supported_langs)

  for (type in types) {

    for (i in sq) {

      lang_i <- supported_langs[[i]]

      el_i <- c(type, lang_i)

      sd_i <- vapply(sd_response_results, get_el_recurse, "", el_i, "character")

      col <- paste(type, lang_i, sep = "_")

      sd_df[[col]] <- sub("^.* \u2013 ", "", sd_i)

    }

  }

  structure(set_translations(sd_df), class = "data.frame")

}

#' @noRd

record_basis <- function() {

  record_bases <- list(enum = "MY.recordBases", fun = get_sysdata)

  get_enumeration(record_bases)

}

#' @noRd

superrecord_basis <- function() {

  options <- c(
    "human_observation",
    "machine_observation",
    "specimen"
  )

  options <- structure(options, class = "translation")

  options <- list(options = options)

  rnms <- c(
    "human_observation_unspecified",
    "machine_observation_unspecified",
    "preserved_specimen"
  )

  structure(options, row.names = rnms, class = "data.frame")

}

#' @noRd

life_stage <- function() {

  animal_life_stages <- list(enum = "MY.lifeStages", fun = get_sysdata)

  animal_life_stages <- get_enumeration(animal_life_stages)

  plant_life_stages <- list(enum = "MY.plantLifeStageEnum", fun = get_sysdata)

  plant_life_stages <- get_enumeration(plant_life_stages)

  life_stages <- rbind(animal_life_stages, plant_life_stages)

  set_translations(life_stages)

}

#' @noRd

sex <- function() {

  sexes <- list(prefix = "MY.sex", suffix = "es")

  sexes <- list(enum = sexes, fun = get_code)

  get_enumeration(sexes)

}

#' @noRd

restriction_reason <- function() {

  reasons <- list(enum = "MZ.secureReason", fun = get_sysdata)

  get_enumeration(reasons)

}

#' @noRd

restriction_level <- function() {

  levels <- list(enum = "MX.secureLevels", fun = get_sysdata)

  get_enumeration(levels)

}

#' @noRd

quality_issues <- function() {

  options <- c("with_issues", "without_issues", "both")

  options <- structure(options, class = "translation")

  options <- list(options = options)

  rnms <- c("only_issues", "no_issues", "both")

  structure(options, row.names = rnms, class = "data.frame")

}

#' @noRd

collection_quality <- function() {

  quality <- list(enum = "MY.collectionQualityEnum", fun = get_sysdata)

  get_enumeration(quality)

}

#' @noRd

record_quality <- function() {

  quality <- list(enum = "MZ.recordQualityEnum", fun = get_sysdata)

  get_enumeration(quality)

}

#' @noRd

record_reliability <- function() {

  options <- c("reliable", "unassessed", "unreliable")

  options <- structure(options, class = "translation")

  options <- list(options = options)

  rnms <- c("reliable", "undefined", "unreliable")

  structure(options, row.names = rnms, class = "data.frame")

}

#' @noRd

complete_list_type <- function() {

  options <- c(
    "all_species_and_all_breeding",
    "all_species_and_partial_breeding",
    "incomplete",
    "all_species"
  )

  options <- structure(options, class = "translation")

  options <- list(options = options)

  rnms <- c(
    "MY.completeListTypeCompleteWithBreedingStatus",
    "MY.completeListTypeComplete",
    "MY.completeListTypeIncomplete",
    "MY.completeListTypeCompleteWithBreedingStatus,MY.completeListTypeComplete"
  )

  structure(options, row.names = rnms, class = "data.frame")

}
