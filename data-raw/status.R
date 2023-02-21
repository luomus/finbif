source("data-raw/utils.R")

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
  finbif:::api_get(
    list(path = "metadata/ranges", query = list(), cache = FALSE)
  )[["content"]]

redlist_status <- sapply(metadata_ranges[["MX.iucnStatuses"]], getElement, "id")

stopifnot(
  identical(sort(row.names(red_list_status)), sort(redlist_status))
)
