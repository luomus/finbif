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

  request <- list(
    path = paste0("metadata/ranges/", x),
    query = list(lang = "multi"),
    cache = infer_cache(cache)
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

  request <- list(
    path = "warehouse/enumeration-labels",
    cache = infer_cache(cache)
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

  request <- list(
    path = "areas",
    query = list(type = x, lang = "multi", pageSize = 1000L),
    cache = infer_cache(cache)
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

  request <- list(
    path = "informal-taxon-groups",
    query = list(lang = "multi", pageSize = 1000L),
    cache = infer_cache(cache)
  )

  sd_response <- api_get(request)

  sd_response_content <- sd_response[[c("content", "results")]]

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
    ML.351 = "Ahvenanmaa",
    ML.352 = "Uusimaa",
    ML.353 = "Ahvenanmaa",
    ML.354 = "Ahvenanmaa",
    ML.355 = "Ahvenanmaa",
    ML.356 = "Ahvenanmaa",
    ML.357 = "Ahvenanmaa",
    ML.358 = "Ahvenanmaa",
    ML.359 = "Ahvenanmaa",
    ML.360 = "Ahvenanmaa",
    ML.361 = "Ahvenanmaa",
    ML.362 = "Uusimaa",
    ML.363 = "Uusimaa",
    ML.364 = "Uusimaa",
    ML.365 = "Uusimaa",
    ML.366 = "Uusimaa",
    ML.367 = "Uusimaa",
    ML.368 = "Ahvenanmaa",
    ML.369 = "Ahvenanmaa",
    ML.370 = "Uusimaa",
    ML.371 = "Uusimaa",
    ML.372 = "Uusimaa",
    ML.373 = "Varsinais-Suomi",
    ML.374 = "Ahvenanmaa",
    ML.375 = "Uusimaa",
    ML.376 = "Kanta-H\u00e4me",
    ML.377 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.378 = "Kanta-H\u00e4me",
    ML.379 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.380 = "Kanta-H\u00e4me",
    ML.381 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.382 = "Uusimaa",
    ML.383 = "Uusimaa",
    ML.384 = "Uusimaa",
    ML.385 = "Uusimaa",
    ML.386 = "Varsinais-Suomi",
    ML.387 = "Varsinais-Suomi",
    ML.388 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.389 = "Uusimaa",
    ML.390 = "Uusimaa",
    ML.391 = "Varsinais-Suomi",
    ML.392 = "Uusimaa",
    ML.393 = "Varsinais-Suomi",
    ML.394 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.396 = "Kymenlaakso",
    ML.397 = "Uusimaa",
    ML.398 = "Uusimaa",
    ML.399 = "Satakunta",
    ML.400 = "Satakunta",
    ML.401 = "Varsinais-Suomi",
    ML.402 = "Varsinais-Suomi",
    ML.403 = "Uusimaa",
    ML.404 = "Kymenlaakso",
    ML.405 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.406 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.407 = "Keski-Suomi",
    ML.408 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.409 = "Satakunta",
    ML.410 = "Uusimaa",
    ML.411 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.412 = "Kymenlaakso",
    ML.413 = "Etel\u00e4-Savo",
    ML.414 = "Pohjois-Karjala",
    ML.415 = "Pirkanmaa",
    ML.416 = "Kymenlaakso",
    ML.417 = "P\u00e4ij\u00e4t-H\u00e4me",
    ML.418 = "Etel\u00e4-Savo",
    ML.419 = "Keski-Suomi",
    ML.420 = "Etel\u00e4-Savo",
    ML.421 = "Etel\u00e4-Savo",
    ML.422 = "Etel\u00e4-Karjala",
    ML.423 = "Etel\u00e4-Savo",
    ML.424 = "Etel\u00e4-Savo",
    ML.425 = "Etel\u00e4-Savo",
    ML.426 = "Etel\u00e4-Karjala",
    ML.427 = "Etel\u00e4-Savo",
    ML.428 = "Etel\u00e4-Karjala",
    ML.429 = "Satakunta",
    ML.430 = "Pirkanmaa",
    ML.431 = "Etel\u00e4-Karjala",
    ML.432 = "Satakunta",
    ML.433 = "Keski-Suomi",
    ML.434 = "Keski-Suomi",
    ML.435 = "Satakunta",
    ML.436 = "Pohjanmaa",
    ML.437 = "Kanta-H\u00e4me",
    ML.438 = "Kanta-H\u00e4me",
    ML.439 = "Pirkanmaa",
    ML.440 = "Pirkanmaa",
    ML.442 = "Pohjois-Savo",
    ML.443 = "Etel\u00e4-Karjala",
    ML.444 = "Pohjois-Karjala",
    ML.445 = "Pohjois-Karjala",
    ML.446 = "Etel\u00e4-Savo",
    ML.447 = "Etel\u00e4-Pohjanmaa",
    ML.449 = "Pohjois-Savo",
    ML.450 = "Etel\u00e4-Savo",
    ML.451 = "Etel\u00e4-Karjala",
    ML.452 = "Pohjois-Karjala",
    ML.453 = "Pohjois-Savo",
    ML.454 = "Pohjois-Savo",
    ML.455 = "Satakunta",
    ML.456 = "Varsinais-Suomi",
    ML.457 = "Varsinais-Suomi",
    ML.458 = "Varsinais-Suomi",
    ML.459 = "Satakunta",
    ML.460 = "Varsinais-Suomi",
    ML.461 = "Varsinais-Suomi",
    ML.462 = "Varsinais-Suomi",
    ML.464 = "Satakunta",
    ML.465 = "Satakunta",
    ML.466 = "Satakunta",
    ML.467 = "Satakunta",
    ML.468 = "Varsinais-Suomi",
    ML.469 = "Varsinais-Suomi",
    ML.470 = "Kanta-H\u00e4me",
    ML.471 = "Etel\u00e4-Karjala",
    ML.472 = "Varsinais-Suomi",
    ML.473 = "Varsinais-Suomi",
    ML.474 = "Etel\u00e4-Karjala",
    ML.475 = "Varsinais-Suomi",
    ML.476 = "Kymenlaakso",
    ML.477 = "Etel\u00e4-Karjala",
    ML.478 = "Varsinais-Suomi",
    ML.479 = "Pohjanmaa",
    ML.480 = "Pohjanmaa",
    ML.481 = "Varsinais-Suomi",
    ML.482 = "Keski-Pohjanmaa",
    ML.483 = "Pirkanmaa",
    ML.484 = "Etel\u00e4-Pohjanmaa",
    ML.485 = "Etel\u00e4-Pohjanmaa",
    ML.486 = "Varsinais-Suomi",
    ML.487 = "Etel\u00e4-Savo",
    ML.488 = "Etel\u00e4-Savo",
    ML.489 = "Pohjois-Karjala",
    ML.490 = "Pohjanmaa",
    ML.491 = "Pohjanmaa",
    ML.492 = "Pohjanmaa",
    ML.493 = "Pohjanmaa",
    ML.494 = "Keski-Pohjanmaa",
    ML.495 = "Pohjois-Savo",
    ML.496 = "Varsinais-Suomi",
    ML.497 = "Pirkanmaa",
    ML.498 = "Etel\u00e4-Pohjanmaa",
    ML.499 = "Pohjanmaa",
    ML.500 = "Etel\u00e4-Pohjanmaa",
    ML.501 = "Etel\u00e4-Pohjanmaa",
    ML.502 = "Keski-Suomi",
    ML.503 = "Etel\u00e4-Pohjanmaa",
    ML.504 = "Keski-Suomi",
    ML.505 = "Keski-Suomi",
    ML.506 = "Etel\u00e4-Pohjanmaa",
    ML.507 = "Keski-Suomi",
    ML.508 = "Keski-Pohjanmaa",
    ML.509 = "Kanta-H\u00e4me",
    ML.510 = "Satakunta",
    ML.512 = "Etel\u00e4-Pohjanmaa",
    ML.513 = "Pirkanmaa",
    ML.514 = "Pirkanmaa",
    ML.515 = "Keski-Suomi",
    ML.516 = "Keski-Suomi",
    ML.517 = "Pohjois-Pohjanmaa",
    ML.518 = "Keski-Suomi",
    ML.519 = "Pohjanmaa",
    ML.520 = "Pohjois-Pohjanmaa",
    ML.521 = "Pirkanmaa",
    ML.522 = "Etel\u00e4-Pohjanmaa",
    ML.523 = "Pohjanmaa",
    ML.524 = "Pohjanmaa",
    ML.525 = "Pirkanmaa",
    ML.526 = "Keski-Pohjanmaa",
    ML.527 = "Keski-Suomi",
    ML.528 = "Pirkanmaa",
    ML.529 = "Pirkanmaa",
    ML.530 = "Keski-Suomi",
    ML.531 = "Keski-Suomi",
    ML.532 = "Keski-Suomi",
    ML.533 = "Varsinais-Suomi",
    ML.534 = "Keski-Suomi",
    ML.536 = "Keski-Suomi",
    ML.537 = "Kanta-H\u00e4me",
    ML.538 = "Keski-Suomi",
    ML.539 = "Keski-Suomi",
    ML.541 = "Pirkanmaa",
    ML.542 = "Etel\u00e4-Pohjanmaa",
    ML.543 = "Etel\u00e4-Pohjanmaa",
    ML.544 = "Kanta-H\u00e4me",
    ML.545 = "Etel\u00e4-Pohjanmaa",
    ML.546 = "Keski-Suomi",
    ML.547 = "Varsinais-Suomi",
    ML.548 = "Pohjois-Savo",
    ML.549 = "Keski-Suomi",
    ML.550 = "Keski-Suomi",
    ML.551 = "Etel\u00e4-Pohjanmaa",
    ML.552 = "Pirkanmaa",
    ML.553 = "Pohjois-Savo",
    ML.554 = "Pirkanmaa",
    ML.555 = "Pirkanmaa",
    ML.556 = "Etel\u00e4-Pohjanmaa",
    ML.557 = "Etel\u00e4-Pohjanmaa",
    ML.558 = "Pohjois-Savo",
    ML.559 = "Etel\u00e4-Pohjanmaa",
    ML.560 = "Keski-Pohjanmaa",
    ML.561 = "Pirkanmaa",
    ML.562 = "Pirkanmaa",
    ML.563 = "Satakunta",
    ML.564 = "Keski-Pohjanmaa",
    ML.565 = "Keski-Pohjanmaa",
    ML.566 = "Keski-Pohjanmaa",
    ML.567 = "Pohjois-Savo",
    ML.568 = "Kainuu",
    ML.569 = "Pohjois-Savo",
    ML.570 = "Pohjois-Savo",
    ML.572 = "Pohjois-Pohjanmaa",
    ML.573 = "Pohjois-Savo",
    ML.574 = "Pohjois-Savo",
    ML.575 = "Pohjois-Savo",
    ML.576 = "Pohjois-Savo",
    ML.577 = "Pirkanmaa",
    ML.578 = "Pohjois-Karjala",
    ML.579 = "Pirkanmaa",
    ML.580 = "Pohjois-Karjala",
    ML.582 = "Pohjois-Karjala",
    ML.584 = "Pohjois-Karjala",
    ML.585 = "Pohjois-Savo",
    ML.586 = "Pohjois-Pohjanmaa",
    ML.587 = "Pohjois-Pohjanmaa",
    ML.588 = "Pohjois-Pohjanmaa",
    ML.589 = "Kanta-H\u00e4me",
    ML.590 = "Pohjois-Karjala",
    ML.591 = "Pohjois-Pohjanmaa",
    ML.592 = "Kainuu",
    ML.593 = "Pohjois-Pohjanmaa",
    ML.594 = "Pohjois-Pohjanmaa",
    ML.595 = "Pohjois-Pohjanmaa",
    ML.596 = "Kainuu",
    ML.597 = "Pohjois-Pohjanmaa",
    ML.598 = "Pohjois-Pohjanmaa",
    ML.599 = "Pohjois-Savo",
    ML.600 = "Lappi",
    ML.601 = "Pohjois-Pohjanmaa",
    ML.602 = "Pohjois-Pohjanmaa",
    ML.603 = "Lappi",
    ML.604 = "Pohjois-Pohjanmaa",
    ML.605 = "Kainuu",
    ML.606 = "Pohjois-Pohjanmaa",
    ML.607 = "Kainuu",
    ML.608 = "Pohjois-Pohjanmaa",
    ML.609 = "Kainuu",
    ML.610 = "Pohjois-Pohjanmaa",
    ML.611 = "Pohjois-Pohjanmaa",
    ML.612 = "Pohjois-Karjala",
    ML.613 = "Kainuu",
    ML.614 = "Kainuu",
    ML.615 = "Lappi",
    ML.616 = "Pohjois-Pohjanmaa",
    ML.617 = "Lappi",
    ML.618 = "Lappi",
    ML.619 = "Lappi",
    ML.620 = "Lappi",
    ML.621 = "Lappi",
    ML.622 = "Lappi",
    ML.623 = "Lappi",
    ML.624 = "Lappi",
    ML.625 = "Lappi",
    ML.626 = "Lappi",
    ML.627 = "Lappi",
    ML.628 = "Lappi",
    ML.629 = "Lappi",
    ML.630 = "Lappi",
    ML.631 = "Lappi",
    ML.632 = "Lappi",
    ML.633 = "Lappi",
    ML.634 = "Lappi",
    ML.635 = "Kanta-H\u00e4me",
    ML.636 = "Pohjois-Karjala",
    ML.637 = "Pohjois-Karjala",
    ML.638 = "Pohjois-Pohjanmaa",
    ML.639 = "Pohjois-Pohjanmaa",
    ML.640 = "Pohjois-Pohjanmaa",
    ML.641 = "Satakunta",
    ML.642 = "Pohjois-Pohjanmaa",
    ML.643 = "Uusimaa",
    ML.644 = "Pohjois-Pohjanmaa",
    ML.645 = "Pohjois-Pohjanmaa",
    ML.647 = "Varsinais-Suomi",
    ML.648 = "Uusimaa",
    ML.649 = "Satakunta",
    ML.650 = "Pohjois-Pohjanmaa",
    ML.651 = "Ahvenanmaa",
    ML.652 = "Pohjanmaa",
    ML.653 = "Etel\u00e4-Pohjanmaa",
    ML.654 = "Pohjanmaa",
    ML.655 = "Pohjois-Savo",
    ML.656 = "Ahvenanmaa",
    ML.657 = "Pohjois-Pohjanmaa",
    ML.658 = "Pohjois-Pohjanmaa",
    ML.659 = "Pohjois-Savo",
    ML.660 = "Uusimaa",
    ML.661 = "Kymenlaakso",
    ML.662 = "Ahvenanmaa",
    ML.663 = "Pirkanmaa",
    ML.664 = "Varsinais-Suomi",
    ML.665 = "Varsinais-Suomi",
    ML.666 = "Pirkanmaa",
    ML.667 = "Uusimaa",
    ML.668 = "Pirkanmaa",
    ML.669 = "Pohjois-Pohjanmaa",
    ML.670 = "Pohjanmaa"
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

  request <- list(
    path = "sources",
    query = list(lang = "multi", pageSize = 1000L),
    cache = infer_cache(cache)
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

  r <- c(
    "HUMAN_OBSERVATION_UNSPECIFIED",
    "MACHINE_OBSERVATION_UNSPECIFIED",
    "PRESERVED_SPECIMEN"
  )

  bases <- list(
    code = tolower(r),
    name_en =  c(
      "Human Observation",
      "Machine Observation",
      "Specimen"
    ),
    name_fi =  c(
      "Havaittu",
      "Laitteen tekem\u00e4 havainto",
      "N\u00e4yte"
    ),
    name_sv =  c(
      "Observation",
      "Maskinobservation",
      "Prov"
    )
  )

  bases <- structure(bases, row.names = r, class = "data.frame")

  set_translations(bases)

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

  r <- c("ONLY_ISSUES", "NO_ISSUES", "BOTH")

  quality <- list(
    code =  c("with_issues", "without_issues", "both"),
    name_en = c(
      "With issues",
      "Without issues",
      "Both"
    ),
    name_fi = c(
      "Vain virheelliset",
      "Vain virheett\u00f6m\u00e4t",
      "Virheelliset ja virheett\u00f6m\u00e4t"
    ),
    name_sv = c(
      "Endast med problem",
      "Inga problem",
      "Med problem och inga problem"
    )
  )

  quality <- structure(quality, row.names = r, class = "data.frame")

  set_translations(quality)

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

  r <- c("RELIABLE", "UNDEFINED", "UNRELIABLE")

  reliability <- list(
    code = c("reliable", "unassessed", "Unreliable"),
    name_en = c("Reliable", "Unassessed", "Unreliable"),
    name_fi = c("Luotettava", "Neutraali", "Ep\u00e4luotettava"),
    name_sv = c("P\u00e5litlig", "Neutral", "Op\u00e5litliga")
  )

  reliability <- structure(reliability, row.names = r, class = "data.frame")

  set_translations(reliability)

}

#' @noRd

complete_list_type <- function() {

  r <- c(
    "MY.completeListTypeCompleteWithBreedingStatus",
    "MY.completeListTypeComplete",
    "MY.completeListTypeIncomplete",
    "MY.completeListTypeCompleteWithBreedingStatus,MY.completeListTypeComplete"
  )

  list_type <- list(
    code = c(
      "all_species_and_all_breeding",
      "all_species_and_partial_breeding",
      "incomplete",
      "all_species"
    ),
    name_en = c(
      "All species and all breeding codes",
      "All species and some breeding codes",
      "Incomplete",
      "All species"
    ),
    name_fi = c(
      paste0(
        "T\u00e4ydellinen lajilista sis\u00e4lt\u00e4en",
        " pesim\u00e4varmuusindeksit kaikille havainnoille"
      ),
      paste0(
        "T\u00e4ydellinen lajiluettelo sis\u00e4lt\u00e4en",
        " joidenkin havaintojen lis\u00e4\u00e4ntymisvarmuusindeksit"
      ),
      "Ep\u00e4t\u00e4ydellinen lajilista",
      "T\u00e4ydellinen lajilista"
    ),
    name_sv = c(
      paste0(
        "Komplett artlista inklusive h\u00e4ckningss\u00e4kerhetsindex",
        " f\u00f6r alla observationer"
      ),
      paste0(
        "Komplett artlista inklusive h\u00e4ckningss\u00e4kerhetsindex",
        " f\u00f6r vissa observationer"
      ),
      "Komplett artlista",
      "Okomplett artlista"
    )
  )

  list_type <- structure(list_type, row.names = r, class = "data.frame")

  set_translations(list_type)

}
