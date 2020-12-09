administrative_status <- read.csv(
  text = "
    finbif_api_status, translated_status, translated_status_code
    MX.finlex160_1997_appendix4, 'Finlex 160 1997 appendix 4', FNLX160_97_4
    MX.finlex160_1997_appendix4_specialInterest, 'Finlex 160 1997 appendix 4 special interest', FNLX160_97_4SI
    MX.finlex160_1997_appendix2a, 'Finlex 160 1997 appendix 2a', FNLX160_97_2A
    MX.finlex160_1997_appendix2b, 'Finlex 160 1997 appendix 2b', FNLX160_97_2B
    MX.finlex160_1997_appendix3a, 'Finlex 160 1997 appendix 3a', FNLX160_97_3A
    MX.finlex160_1997_appendix3b, 'Finlex 160 1997 appendix 3b', FNLX160_97_3B
    MX.finlex160_1997_appendix3c, 'Finlex 160 1997 appendix 3c', FNLX160_97_3C
    MX.habitatsDirectiveAnnexII, 'Habitats directive annex II', HABDIR2
    MX.habitatsDirectiveAnnexIV, 'Habitats directive annex IV', HABDIR4
    MX.habitatsDirectiveAnnexV, 'Habitats directive annex V', HABDIR5
    MX.primaryInterestInEU, 'Primary interest in EU', PI_EU
    MX.habitatsDirectiveAnnexIIExceptionGranted, 'Habitats directive annex II exception granted', HABDIR2E
    MX.habitatsDirectiveAnnexIVExceptionGranted, 'Habitats directive annex IV exception granted', HABDIR4E
    MX.habitatsDirectiveAnnexVExceptionGranted, 'Habitats directive annex V exception granted', HABDIR5E
    MX.birdsDirectiveStatusAppendix1, 'Birds directive status appendix 1', BDS1
    MX.birdsDirectiveStatusAppendix2A, 'Birds directive status appendix 2a', BDS2A
    MX.birdsDirectiveStatusAppendix2B, 'Birds directive status appendix 2b', BDS2B
    MX.birdsDirectiveStatusAppendix3A, 'Birds directive status appendix 3a', BDS3A
    MX.birdsDirectiveStatusAppendix3B, 'Birds directive status appendix 3b', BDS3B
    MX.finnishEnvironmentInstitute2010protectionPrioritySpecies, 'Finnish environment institute 2010 protection priority species', FEI_PPS
    MX.finnishEnvironmentInstitute2020conservationProjectSpecies, 'Finnish environment institute 2020 conservation project species', FEI_CPS
    MX.finnishEnvironmentInstitute2020conservationProjectAapamireSpecies, 'Finnish environment institute 2020 conservation project aapamire species', FEI_CPAS
    MX.gameBird, 'Game bird', GMEB
    MX.gameMammal, 'Game mammal', GMEM
    MX.unprotectedSpecies, 'Unprotected species', UNP_SP
    MX.nationallySignificantInvasiveSpecies, 'Nationally significant invasive species', NS_INVSV
    MX.euInvasiveSpeciesList, 'EU invasive species list', EU_INVSV
    MX.otherPlantPest, 'Other plant pest' OPP_INVSV
    MX.qualityPlantPest, 'Quality plant pest', QLPP_INVSV
    MX.quarantinePlantPest, 'Quarantine plant pest', QPP_INVSV
    MX.nationalInvasiveSpeciesStrategy, 'National invasive species strategy', NSS_INVSV
    MX.otherInvasiveSpeciesList, 'Other invasive species list', OTH_INVSV
    MX.controllingRisksOfInvasiveAlienSpecies, 'Controlling risks of invasive alien species', CRAS_INVSV
    MX.finnishEnvironmentInstitute20072010forestSpecies, 'Finnish environment institute 2007-2010 forest species', FEI_FS
    MX.cropWildRelative, 'Crop wild relative', CWR
  ",
  stringsAsFactors = FALSE, strip.white = TRUE, row.names = 1L, quote = "'"
)

class(administrative_status[["translated_status_code"]]) <- "translation"

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
  identical(sort(row.names(administrative_status)), sort(admin_status))
)

redlist_status <- sapply(metadata_ranges[["MX.iucnStatuses"]], getElement, "id")

stopifnot(
  identical(sort(row.names(red_list_status)), sort(redlist_status))
)
