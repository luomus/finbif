regulatory_status <- read.csv(
  # nolint start: line_len
  text = "
    finbif_api_status, status_code, description_en, description_fi, description_sv
    MX.finlex160_1997_appendix4, FNLX160_97_4
    MX.finlex160_1997_appendix4_specialInterest, FNLX160_97_4_SI
    MX.finlex160_1997_appendix2a, FNLX160_97_2A
    MX.finlex160_1997_appendix2b, FNLX160_97_2B
    MX.finlex160_1997_appendix3a, FNLX160_97_3A
    MX.finlex160_1997_appendix3b, FNLX160_97_3B
    MX.finlex160_1997_appendix3c, FNLX160_97_3C
    MX.habitatsDirectiveAnnexII, HABDIR2
    MX.habitatsDirectiveAnnexIV, HABDIR4
    MX.habitatsDirectiveAnnexV, HABDIR5
    MX.primaryInterestInEU, PI_EU
    MX.habitatsDirectiveAnnexIIExceptionGranted, HABDIR2E
    MX.habitatsDirectiveAnnexIVExceptionGranted, HABDIR4E
    MX.habitatsDirectiveAnnexVExceptionGranted, HABDIR5E
    MX.birdsDirectiveStatusAppendix1, BDS1
    MX.birdsDirectiveStatusAppendix2A, BDS2A
    MX.birdsDirectiveStatusAppendix2B, BDS2B
    MX.birdsDirectiveStatusAppendix3A, BDS3A
    MX.birdsDirectiveStatusAppendix3B, BDS3B
    MX.finnishEnvironmentInstitute2010protectionPrioritySpecies, FEI_PPS
    MX.finnishEnvironmentInstitute2020conservationProjectSpecies, FEI_CPS
    MX.finnishEnvironmentInstitute2020conservationProjectAapamireSpecies, FEI_CPAS
    MX.gameBird, GMEB
    MX.gameMammal, GMEM
    MX.unprotectedSpecies, UNP_SP
    MX.nationallySignificantInvasiveSpecies, NS_INVSV
    MX.euInvasiveSpeciesList, EU_INVSV
    MX.otherPlantPest, OPP_INVSV
    MX.qualityPlantPest, QLPP_INVSV
    MX.quarantinePlantPest, QPP_INVSV
    MX.nationalInvasiveSpeciesStrategy, NSS_INVSV
    MX.otherInvasiveSpeciesList, OTH_INVSV
    MX.controllingRisksOfInvasiveAlienSpecies, CRAS_INVSV
    MX.finnishEnvironmentInstitute20072010forestSpecies, FEI_FS
    MX.finnishEnvironmentInstitute20192021forestSpecies, FEI_FS2
    MX.cropWildRelative, CWR
    MX.birdsDirectiveStatusMigratoryBirds, BDSMB
    MX.cites_appendixI, CITES1
    MX.cites_appendixII, CITES2
    MX.cites_appendixIII, CITES3
    MX.euRegulation_cites_appendixA, EU_CITESA
    MX.euRegulation_cites_appendixB, EU_CITESB
    MX.finlex160_1997_appendix4_2021, FNLX160_97_4_2021
    MX.finlex160_1997_appendix4_specialInterest_2021, FNLX160_97_4_SI_2021
    MX.finlex160_1997_largeBirdsOfPrey, FNLX160_97_LBP
    MX.finlex160_1997_appendix1, FNLX160_97_1
    MX.finnishEnvironmentInstitute2020protectionPrioritySpecies, FEI2020PPS
    MX.finnishEnvironmentInstitute2020conservationProjectVascularSpecies, FEI2020CPVPS
    MX.habitatsDirectiveAnnexII_FinlandNaturaSpecies, HABDIR2FN
    MX.euRegulation_cites_appendixD, EU_CITESD
  ",
  # nolint end
  stringsAsFactors = FALSE, strip.white = TRUE, row.names = 1L, quote = "'"
)


for (i in row.names(regulatory_status)) {

  status <- httr::GET(paste0("https://tun.fi/", i))
  status <- httr::content(status)

  for (j in status[["label"]]) {

    regulatory_status[i, paste0("description_", j[["@language"]])] <- j[["@value"]]

  }

}

class(regulatory_status[["status_code"]]) <- "translation"

red_list_status <- read.csv(
  text = "
    finbif_api_status, translated_status, translated_status_code
    MX.iucnEX, 'Extinct', 'EX'
    MX.iucnEW, 'Extinct in the Wild', 'EW'
    MX.iucnRE, 'Regionally Extinct', 'RE'
    MX.iucnCR, 'Critically Endangered', 'CR'
    MX.iucnEN, 'Endangered', 'EN'
    MX.iucnVU, 'Vulnerable', 'VU'
    MX.iucnNT, 'Near Threatened', 'NT'
    MX.iucnLC, 'Least Concern', 'LC'
    MX.iucnDD, 'Data Deficient', 'DD'
    MX.iucnNA, 'Not Applicable', 'NA'
    MX.iucnNE, 'Not Evaluated', 'NE'
  ",
  stringsAsFactors = FALSE, strip.white = TRUE, row.names = 1L, quote = "'",
  na.strings = " "
)

class(red_list_status[["translated_status_code"]]) <- "translation"
class(red_list_status[["translated_status"]]) <- "translation"

metadata_ranges <-
  finbif:::api_get("metadata/ranges", list(), FALSE)[["content"]]

admin_status <-
  sapply(metadata_ranges[["MX.adminStatusEnum"]], getElement, "id")

stopifnot(
  identical(sort(row.names(regulatory_status)), sort(admin_status))
)

redlist_status <- sapply(metadata_ranges[["MX.iucnStatuses"]], getElement, "id")

stopifnot(
  identical(sort(row.names(red_list_status)), sort(redlist_status))
)
