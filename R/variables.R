#' FinBIF record variables
#'
#' FinBIF record variables that can be selected in finbif occurrence search.
#'
#' @section Taxa:
#' Variables related to taxonomy of records include:
#'
#' - `scientific_name` Character.
#'   Scientific name of taxon.
#' - `taxon_rank` Character.
#'   Taxonomic rank of the taxon (in the form of a URI).
#' - `author`
#' - `is_unidentifiable`
#' - `orig_taxon_checklist`
#' - `orig_taxon_cursive`
#' - `orig_taxon_finnish`
#' - `orig_taxon_id`
#' - `orig_informal_groups`
#' - `orig_qname`
#' - `orig_taxon_scientific_name`
#' - `orig_scientific_name_author`
#' - `orig_scientific_display_name`
#' - `orig_taxon_rank`
#' - `orig_name_vernacular`
#' - `taxon_checklist`
#' - `taxon_name_cursive`
#' - `taxon_finnish`
#' - `taxon_id`
#' - `informal_groups`
#' - `qname`
#' - `scientific_name_author`
#' - `scientific_display_name`
#' - `vernacular_name`
#' - `annotated_taxon_id`
#' - `taxon_verbatim`
#' - `reported_informal_group`
#' - `reported_taxon_id`
#'
#' @section Abundance, sex & life history:
#' Variables related to abundance, sex and life history include:
#'
#' - `abundance` Integer.
#'   Number of individuals recorded or inferred from the record. Note that many
#'   records with `abundance = 1` only indicate the record is of one individual
#'   and may not necessarily imply that this was the abundance at that specified
#'   place and time (e.g., a preserved museum specimen consisting of a single
#'   individual).
#' - `{female|male}_abundance`
#' - `pair_abundance`
#' - `abundance_verbatum`
#' - `life_stage`
#' - `sex`
#'
#' @section Location:
#' Variables related to the location of record include:
#'
#' - `country`
#' - `province`
#' - `municipality`
#' - `lat_wgs84`
#' - `lon_wgs84`
#' - `wkt_wgs84`
#' - `bird_assoc_area`
#' - `bird_assoc_area_id`
#' - `location_collection_id`
#' - `location_id`
#' - `location_municipality`
#' - `location_municipality_id`
#' - `location_name`
#' - `location_lat_kkj`
#' - `location_lon_kkj`
#' - `location_id`
#' - `line_length_m`
#' - `area_m2`
#' - `lat_max_euref`
#' - `lat_min_euref`
#' - `lon_max_euref`
#' - `lon_min_euref`
#' - `wkt_euref`
#' - `lat_max_wgs84`
#' - `lat_min_wgs84`
#' - `lon_max_wgs84`
#' - `lon_min_wgs84`
#' - `lat_005_wgs84`
#' - `lon_005_wgs84`
#' - `lat_01_wgs84`
#' - `lon_01_wgs84`
#' - `lat_05_wgs84`
#' - `lon_05_wgs84`
#' - `lat_1_wgs84`
#' - `lon_1_wgs84`
#' - `lat_max_kkj`
#' - `lat_min_kkj`
#' - `lon_max_kkj`
#' - `lon_min_kkj`
#' - `lat_100_kkj`
#' - `lon_100_kkj`
#' - `lat_100_center_kkj`
#' - `lon_100_center_kkj`
#' - `lat_10_kkj`
#' - `lon_10_kkj`
#' - `lat_10_center_kkj`
#' - `lon_10_center_kkj`
#' - `lat_1_kkj`
#' - `lon_1_kkj`
#' - `lat_1_center_kkj`
#' - `lon_1_center_kkj`
#' - `lat_50_kkj`
#' - `lon_50_kkj`
#' - `lat_50_center_kkj`
#' - `lon_50_center_kkj`
#' - `wkt_kkj`
#' - `coordinates_verbatim`
#' - `country_verbatim`
#' - `higher_geography`
#' - `province_id`
#' - `province_ids`
#' - `coordinate_accuracy`
#' - `country_id`
#' - `municipalities_ids`
#' - `municipality_id`
#' - `province_source`
#' - `coordinates_source`
#' - `country_source`
#' - `municipality_source`
#' - `observers_name`
#' - `observers_id`
#' - `observers_user_id`
#' - `locality`
#' - `municipality_verbatum`
#' - `province_verbatum`
#' - `province_verbatim2`
#' - `is_breeding_location`
#'
#' @section Time:
#' Variables related to time of record include:
#'
#' - `date_start`
#' - `date_end`
#' - `hour_start`
#' - `hour_end`
#' - `minute_start`
#' - `minute_end`
#' - `formatted_date_time`
#' - `century`
#' - `decade`
#' - `year`
#' - `month`
#' - `day`
#' - `ordinal_day_start`
#' - `ordinal_day_end`
#' - `season_start`
#' - `season_end`
#' - `date_created`
#' - `first_load_date`
#' - `modified_date`
#' - `load_date`
#'
#' @section Data restrictions:
#' Variables related to restricted records include:
#'
#' - `restricted`
#' - `restriction_level`
#' - `restriction_reasons`
#' - `conservation_reason_restricted`
#' - `custom_reason_restricted`
#' - `data_quarantine_period_reason_restricted`
#'
#' @section Data quality:
#' Variables related to the quality of records include:
#'
#' - `any_issues`
#' - `record_issue`
#' - `record_issue_message`
#' - `record_issue_source`
#' - `record_reliable`
#' - `taxon_issue_message`
#' - `taxon_reliability`
#' - `taxon_issue_source`
#' - `event_issue`
#' - `event_issue_message`
#' - `event_issue_source`
#' - `location_issue`
#' - `location_issue_message`
#' - `location_issue_source`
#' - `time_issue`
#' - `time_issue_message`
#' - `time_issue_source`
#' - `document_issue`
#' - `document_issue_message`
#' - `document_issue_source`
#' - `document_reliablity`
#' - `reported_taxon_confidence`
#'
#' @section Media:
#' Variables related to media (images, audio, etc.,) associated with records
#' include:
#'
#' - `record_media_author`
#' - `record_media_caption`
#' - `record_media_copyright`
#' - `record_media_url`
#' - `record_media_license_abbr`
#' - `record_media_license_id`
#' - `record_media_type`
#' - `record_thumbnail_square`
#' - `record_thumbnail`
#' - `record_media_count`
#' - `document_media_author`
#' - `document_media_caption`
#' - `document_media_copyright`
#' - `document_media_url`
#' - `document_media_license_abbr`
#' - `document_media_license_id`
#' - `document_media_type`
#' - `document_thumbnail_square`
#' - `document_thumbnail`
#' - `document_media_count`
#' - `event_media_author`
#' - `event_media_caption`
#' - `event_media_copyright`
#' - `event_media_url`
#' - `event_media_license_abbr`
#' - `event_media_license_id`
#' - `event_media_type`
#' - `event_thumbnail_square`
#' - `event_thumbnail`
#' - `event_media_count`
#'
#' @section Identifiers:
#' Variables related to record identifiers include:
#'
#' - `record_id`
#' - `individual_id`
#' - `event_id`
#' - `document_id`
#' - `form_id`
#' - `collection_id`
#' - `source_id`
#'
#' @section Misc:
#' Other variables:
#' - `annotation_count`
#' - `annotation_person`
#' - `annotation_person_name`
#' - `annotation_system`
#' - `annotation_system_name`
#' - `annotation_class`
#' - `annotation_created`
#' - `annotation_id`
#' - `annotation_invasive_control_effectiveness`
#' - `annotation_notes`
#' - `annotation_opinion`
#' - `annotation_type`
#' - `determiner`
#' - `record_fact_decimal`
#' - `record_fact`
#' - `record_fact_integer`
#' - `record_fact_value`
#' - `invasive_control_effectiveness`
#' - `invasive_control`
#' - `record_notes`
#' - `record_basis`
#' - `reference_publication`
#' - `superrecord_basis`
#' - `type_specimen`
#' - `record_order`
#' - `wild_status`
#' - `editor_user_ids`
#' - `document_fact_decimal`
#' - `document_fact_content`
#' - `document_fact_integer`
#' - `document_fact_value`
#' - `keywords`
#' - `license`
#' - `editor_name`
#' - `editor_id`
#' - `editor_user_id`
#' - `document_notes`
#' - `partial`
#' - `event_fact_decimal`
#' - `event_fact`
#' - `event_fact_integer`
#' - `event_fact_value`
#' - `event_order`
#' - `event_notes`
#' - `observer_user_ids`
#' - `taxon_cenus_id`
#' - `taxon_census_type`
#' - `team`
#'
#' @name variables
NULL
