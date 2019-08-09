filter_translations <- read.csv(text = "
  finbif_api_filter, translated_filter, type, translated_values
  taxonId, taxon_id, uri, FALSE
  useIdentificationAnnotations, quality_controlled_det, logical, FALSE
  includeSubTaxa, subtaxa, logical, FALSE
  target, taxon_name, character, FALSE
  includeNonValidTaxa, invalid_taxa, logical, FALSE
  informalTaxonGroupId, informal_group, uri, TRUE
  informalTaxonGroupIdIncludingReported, informal_group_reported, uri, TRUE
  administrativeStatusId, administrative_status, uri, TRUE
  redListStatusId, red_list_status, uri, TRUE
  typeOfOccurrenceId, occurrence_type, uri, FALSE
  typeOfOccurrenceIdNot, not_occurrence_type, uri, FALSE
  primaryHabitat, primary_habitat, factor, TRUE
  anyHabitat, primary_secondary_habitat, factor, TRUE
  finnish, finnish, logical, FALSE
  invasive, invasive, logical, FALSE
  taxonRankId, taxon_rank, uri, TRUE
  countryId, country, uri, TRUE
  finnishMunicipalityId, municipality, uri, TRUE
  biogeographicalProvinceId, province, uri, TRUE
  area, locality, character, FALSE
  birdAssociationAreaId, bird_assoc_area, uri, FALSE
  namedPlaceId, place_name_id, uri, FALSE
  formId, form_id, uri, FALSE
  time, date_range_ymd, date, FALSE
  yearMonth, date_range_ym, date, FALSE
  dayOfYear, date_range_d, date, FALSE
  season, date_range_md, date, FALSE
  keyword, keywords, character, FALSE
  collectionId, collection, uri, FALSE
  includeSubCollections, subcollections, logical, FALSE
  collectionIdNot, not_collection, uri, FALSE
  reliabilityOfCollection, collection_reliability, integer, FALSE
  sourceId, source, uri, FALSE
  recordBasis, record_basis, factor, FALSE
  superRecordBasis, super_record_basis, factor, FALSE
  lifeStage, life_stage, factor, FALSE
  sex, sex, factor, FALSE
  invasiveControl, invasive_control, factor, FALSE
  invasiveControlled, invasive_controlled, logical, FALSE
  gatheringId, event_id, uri, FALSE
  documentId, document_id, uri, FALSE
  unitId, record_id, uri, FALSE
  individualId, individual_id, uri, FALSE
  individualCountMin, abundance_min, integer, FALSE
  individualCountMax, abundance_max, integer, FALSE
  loadedSameOrAfter, last_import_date_min, date, FALSE
  loadedSameOrBefore, last_import_date_max, date, FALSE
  firstLoadedSameOrAfter, first_import_date_min, date, FALSE
  firstLoadedSameOrBefore, first_import_date_max, date, FALSE
  coordinates, coordinates_area, character, FALSE
  coordinateAccuracyMax, coordinate_accuracy_max, integer, FALSE
  wgs84CenterPoint, coordinates_center, character, FALSE
  ykj1km, coordinates_cell_1k, character, FALSE
  ykj10km, coordinates_cell_10k, character, FALSE
  ykj50km, coordinates_cell_50k, character, FALSE
  ykj100km, coordinates_cell_100k, character, FALSE
  ykj1kmCenter, coordinates_cell_1k_center, character, FALSE
  ykj10kmCenter, coordinates_cell_10k_center, character, FALSE
  ykj50kmCenter, coordinates_cell_50k_center, character, FALSE
  ykj100kmCenter, coordinates_cell_100k_center, character, FALSE
  sourceOfCoordinates, coordinate_source, factor, FALSE
  typeSpecimen, type_specimen, logical, FALSE
  nativeOccurrence, is_native, logical, FALSE
  wild, wild_status, factor, FALSE
  breedingSite, is_breeding_location, logical, FALSE
  hasDocumentMedia, has_document_media, logical, FALSE
  hasGatheringMedia, has_event_media, logical, FALSE
  hasUnitMedia, has_record_media, logical, FALSE
  hasMedia, has_media, logical, FALSE
  editorId, editor_id, uri, FALSE
  observerId, observer_id, uri, FALSE
  editorOrObserverId, editor_or_observer_id, uri, FALSE
  teamMember, event_observer_name, character, FALSE
  teamMemberId, event_observer_id, character, FALSE
  secureReason, secure_reason, factor, FALSE
  secureLevel, secure_level, factor, FALSE
  secured, secure, logical, FALSE
  annotated, annotated, logical, FALSE
  qualityIssues, quality_issues, factor, FALSE
  reliable, reliable, logical, FALSE
  taxonReliability, taxon_reliability, factor, FALSE
  unidentified, unidentified, logical, FALSE
  taxonCensus, taxon_census, uri, FALSE
  unitFact, record_fact, character, FALSE
  gatheringFact, event_fact, character, FALSE
  documentFact, document_fact, character, FALSE
", stringsAsFactors = FALSE, strip.white = TRUE, row.names = 1L)

class(filter_translations[["translated_filter"]]) <- "translation"

filters <- names(
  finbif:::finbif_api_get("warehouse/filters", list(), FALSE)[["content"]]
)

stopifnot(identical(sort(row.names(filter_translations)), sort(filters)))
