#' Get FinBIF records
#'
#' Download filtered records from FinBIF.
#'
#' @param filters List of named character vectors. Filters to apply to records.
#' @param fields Character vector. Columns to return.
#' @param n Integer. How many records to download per page.
#' @param page Integer. Which page of records to download.
#' @return A `finbif_api` object.
#' @export

finbif_records <- function(filters = NULL, fields, n = 10, page = 1) {
  path <- "v0/warehouse/query/list"
  if (missing(fields)) fields <- default_fields
  fields <- paste(fields, collapse = ",")
  filters <- lapply(filters, paste, collapse = ",")
  query <- list(
    page = page,
    pageSize = n,
    selected = fields
  )
  query <- c(query, filters)
  finbif_api_get(path, query)
}

default_fields <-c(
  taxon                  = "unit.linkings.taxon.scientificName",
  taxon_id               = "unit.linkings.taxon.id",
  taxonomic_rank         = "unit.linkings.taxon.taxonRank",
  record_id              = "unit.unitId",
  individual_id          = "unit.individualId",
  record_set_id          = "gathering.gatheringId",
  collection_id          = "document.collectionId",
  source_id              = "document.sourceId",
  observer_id            = "gathering.observerUserIds",
  date_begin             = "gathering.eventDate.begin",
  date_end               = "gathering.eventDate.end",
  hour_begin             = "gathering.hourBegin",
  hour_end               = "gathering.hourEnd",
  minute_begin           = "gathering.minutesBegin",
  minute_end             = "gathering.minutesEnd",
  latitude               = "gathering.conversions.wgs84CenterPoint.lat",
  longitude              = "gathering.conversions.wgs84CenterPoint.lon",
  coordinate_accuracy    = "gathering.interpretations.coordinateAccuracy",
  latitude_min           = "gathering.conversions.wgs84.latMin",
  latitude_max           = "gathering.conversions.wgs84.latMax",
  longitude_min          = "gathering.conversions.wgs84.lonMin",
  longitude_max          = "gathering.conversions.wgs84.lonMax",
  location_wkt           = "gathering.conversions.wgs84WKT",
  count                  = "unit.interpretations.individualCount",
  life_stage             = "unit.lifeStage",
  sex                    = "unit.sex",
  is_native              = "unit.nativeOccurrence",
  is_breeding            = "unit.breedingSite",
  document_issue         = "document.quality.issue.issue",
  collection_reliability = "document.quality.reliabilityOfCollection",
  record_set_issue       = "gathering.quality.issue.issue",
  time_issue             = "gathering.quality.timeIssue.issue",
  location_issue         = "gathering.quality.locationIssue.issue",
  record_issue           = "unit.quality.issue.issue",
  taxon_reliability      = "unit.quality.taxon.reliability",
  has_issue              = "unit.quality.documentGatheringUnitQualityIssues",
  is_reliable            = "unit.quality.reliable"
)
