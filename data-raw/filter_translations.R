filter_translations <- read.csv(text = "
  finbif_api_filter, translated_filter, type
  taxonId, taxon_id, uri
  useIdentificationAnnotations, quality_controlled_det, logical
  includeSubTaxa, subtaxa, logical
  target, taxon_name, character
  includeNonValidTaxa, invalid_taxa, logical
  informalTaxonGroupId, informal_group, uri
  informalTaxonGroupIdIncludingReported, informal_group_reported, uri
  administrativeStatusId, administrative_status, uri
  redListStatusId, redlist_status, uri
  typeOfOccurrenceId, occurrence_type, uri
  typeOfOccurrenceIdNot, not_occurrence_type, uri
  primaryHabitat, primary_habitat, factor
  anyHabitat, primary_secondary_habitat, factor
  finnish, finnish, logical
  invasive, invasive, logical
  taxonRankId, taxon_rank, uri
  countryId, country, uri
  finnishMunicipalityId, municipality, uri
  biogeographicalProvinceId, province, uri
  area, locality, character
  birdAssociationAreaId, bird_assoc_area, uri
  namedPlaceId, place_name_id, uri
  formId, form_id, uri
  time, date_range_ymd, date
  yearMonth, date_range_ym, date
  dayOfYear, date_range_d, date
  season, date_range_md, date
  keyword, keywords, character
  collectionId, collection, uri
  includeSubCollections, subcollections, logical
  collectionIdNot, not_collection, uri
  reliabilityOfCollection, collection_reliability, integer
  sourceId, source, uri
  recordBasis, record_basis, factor
  superRecordBasis, super_record_basis, factor
  lifeStage, life_stage, factor
  sex, sex, factor
  invasiveControl, invasive_control, factor
  invasiveControlled, invasive_controlled, logical
  gatheringId, event_id, uri
  documentId, document_id, uri
  unitId, record_id, uri
  individualId, individual_id, uri
  individualCountMin, abundance_min, integer
  individualCountMax, abundance_max, integer
  loadedSameOrAfter, last_import_date_min, date
  loadedSameOrBefore, last_import_date_max, date
  firstLoadedSameOrAfter, first_import_date_min, date
  firstLoadedSameOrBefore, first_import_date_max, date
  coordinates, coordinates_area, character
  coordinateAccuracyMax, coordinate_accuracy_max, integer
  wgs84CenterPoint, coordinates_center, character
  ykj1km, coordinates_cell_1k, character
  ykj10km, coordinates_cell_10k, character
  ykj50km, coordinates_cell_50k, character
  ykj100km, coordinates_cell_100k, character
  ykj1kmCenter, coordinates_cell_1k_center, character
  ykj10kmCenter, coordinates_cell_10k_center, character
  ykj50kmCenter, coordinates_cell_50k_center, character
  ykj100kmCenter, coordinates_cell_100k_center, character
  sourceOfCoordinates, coordinate_source, factor
  typeSpecimen, type_specimen, logical
  nativeOccurrence, is_native, logical
  wild, wild_status, factor
  breedingSite, is_breeding_location, logical
  hasDocumentMedia, has_document_media, logical
  hasGatheringMedia, has_event_media, logical
  hasUnitMedia, has_record_media, logical
  hasMedia, has_media, logical
  editorId, editor_id, uri
  observerId, observer_id, uri
  editorOrObserverId, editor_or_observer_id, uri
  teamMember, event_observer_name, character
  teamMemberId, event_observer_id, character
  secureReason, secure_reason, factor
  secureLevel, secure_level, factor
  secured, secure, logical
  annotated, annotated, logical
  qualityIssues, quality_issues, factor
  reliable, reliable, logical
  taxonReliability, taxon_reliability, factor
  unidentified, unidentified, logical
  taxonCensus, taxon_census, uri
  unitFact, record_fact, character
  gatheringFact, event_fact, character
  documentFact, document_fact, character
", stringsAsFactors = FALSE, strip.white = TRUE, row.names = 1L)

filters <- names(
  finbif:::finbif_api_get("v0/warehouse/filters", list())$content
)

stopifnot(
  identical(
    sort(row.names(filter_translations)), sort(filters)
  )
)
