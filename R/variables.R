#' FinBIF record variables
#'
#' FinBIF record variables that can be selected in a finbif occurrence search.
#'
#' @section Identifiers:
#' All identifiers are returned in the form of a URI. Identifiers include:
#'
#' - `record_id`
#'   Record ID. The ID of a record of organism's occurrence at a time and
#'   place.
#' - `individual_id`
#'   ID of an individual organism (e.g., a ringed bird that has been captured
#'   multiple times will have a single `individual_id` and multiple `record_id`s
#'   corresponding to each capture).
#' - `event_id`
#'   Event ID. An event can contain one or more records (e.g., a survey of
#'   plants at a particular location and time.)
#' - `document_id`
#'   Document ID. A set of events that share common metadata.
#' - `form_id`
#'   Form ID. The form used to create the document, event, record data.
#' - `collection_id`
#'   Collection ID. All documents, events, and records belong to a collection (
#'   e.g., a museum collection, or the datasets collected by a specific
#'   institution). Collections themselves can be part of a larger
#'   (super)collection (e.g., all the collections at specific museum). Only the
#'   lowest level collection ID for a record is returned. Use
#'   `finbif_collections()` to explore the hierarchy of collections.
#' - `source_id`
#'   Source ID. The source of the collection's data.
#'
#' @section Taxa:
#' Variables related to taxonomy of records include:
#'
#' - `taxon_id` Character.
#'   The taxon ID in the form of a URI.
#' - `orig_taxon_id` Character.
#'   The taxon ID (above) before (if any) annotation.
#' - `annotated_taxon_id` Charactio
#'   The new taxon ID if the record has had it's taxonomy annotated.
#' - `reported_taxon_id`
#'   The taxon ID reported by the original data provider (may not exist).
#' - `scientific_name` Character.
#'   Scientific name of taxon.
#' - `orig_scientific_name`
#' - `scientific_display_name`
#' - `orig_scientific_display_name`
#' - `vernacular_name`
#' - `orig_name_vernacular.
#' - `taxon_name_cursive`
#' - `orig_taxon_cursive`
#' - `taxon_verbatim`
#' - `scientific_name_author`
#' - `orig_scientific_name_author`
#' - `author_verabtum`
#' - `taxon_rank` Character.
#'   Taxonomic rank of the taxon (in the form of a URI).
#' - `orig_taxon_rank`
#' - `informal_groups`
#' - `orig_informal_groups`
#' - `reported_informal_group`
#' - `taxon_checklist`
#' - `orig_taxon_checklist`
#' - `taxon_finnish`
#' - `orig_taxon_finnish`
#' - `is_unidentifiable`
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
#' - `{lat|lon}_wgs84`
#' - `{lat|lon}_{min|max}_{euref|kkj|wgs84}`
#' - `{lat|lon}_{005|01|05|1}_wgs84`
#' - `{lat|lon}_{1|10|50|100}_kkj`
#' - `{lat|lon}_{1|10|50|100}_center_kkj`
#' - `wkt_{euref|kkj|wgs84}`
#' - `coordinates_verbatim`
#' - `coordinate_accuracy`
#' - `coordinates_source`
#' - `country`
#' - `country_id`
#' - `country_source`
#' - `country_verbatim`
#' - `province`
#' - `province_id`
#' - `province_ids`
#' - `province_source`
#' - `province_verbatum`
#' - `province_verbatim2`
#' - `municipality`
#' - `municipality_id`
#' - `municipality_ids`
#' - `municipality_source`
#' - `municipality_verbatum`
#' - `locality`
#' - `location_id`
#' - `higher_geography`
#' - `line_length_m`
#' - `area_m2`
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
#' - `ordinal_day_start`
#' - `ordinal_day_end`
#' - `season_start`
#' - `season_end`
#' - `formatted_date_time`
#' - `century`
#' - `decade`
#' - `year`
#' - `month`
#' - `day`
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
#' - `record_reliable`
#' - `document_reliablity`
#' - `taxon_reliability`
#' - `taxon_reliability_message`
#' - `taxon_reliability_source`
#' - `reported_taxon_confidence`
#' - `{document|time|location|event|record}_issue`
#' - `{document|time|location|event|record}_issue_message`
#' - `{document|time|location|event|record}_issue_source`
#'
#' @section Media:
#' Variables related to media (images, audio, etc.,) associated with records
#' include:
#'
#' - `{document|event|record}_media_count`
#' - `{document|event|record}_media_author`
#' - `{document|event|record}_media_caption`
#' - `{document|event|record}_media_copyright`
#' - `{document|event|record}_media_url`
#' - `{document|event|record}_media_license_abbr`
#' - `{document|event|record}_media_license_id`
#' - `{document|event|record}_media_type`
#' - `{document|event|record}_thumbnail_square`
#' - `{document|event|record}_thumbnail`
#'
#' @section Misc:
#' Other variables:
#' - `observers_name`
#' - `observers_id`
#' - `observers_user_id`
#' - `observer_user_ids`
#' - `editor_name`
#' - `editor_id`
#' - `editor_user_id`
#' - `determiner`
#' - `team`
#' - `{document|event|record}_fact_decimal`
#' - `{document|event|record}_fact_content`
#' - `{document|event|record}_fact_integer`
#' - `{document|event|record}_fact_value`
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
#' - `keywords`
#' - `license`
#' - `document_notes`
#' - `partial`
#' - `event_order`
#' - `event_notes`
#' - `taxon_cenus_id`
#' - `taxon_census_type`
#' - `record_annotation_count`
#' - `{document|record}_annotation_person_id`
#' - `{document|record}_annotation_person_name`
#' - `{document|record}_annotation_system_id`
#' - `{document|record}_annotation_system_name`
#' - `{document|record}_annotation_class`
#' - `{document|record}_annotation_created`
#' - `{document|record}_annotation_id`
#' - `{document|record}_annotation_invasive_control_effectiveness`
#' - `{document|record}_annotation_notes`
#' - `{document|record}_annotation_opinion`
#' - `{document|record}_annotation_root_id`
#' - `{document|record}_annotation_target_id`
#' - `{document|record}_annotation_type`
#' - `keywords`
#' - `sample_count`
#' - `sample_collection_id`
#' - `sample_fact_decimal`
#' - `sample_fact_content`
#' - `sample_fact_integer`
#' - `sample_fact_value`
#' - `sample_keywords`
#' - `sample_multiple`
#' - `sample_notes`
#' - `sample_quality`
#' - `sample_id`
#' - `sample_order`
#' - `sample_status`
#' - `samples_type`
#'
#' @name variables
NULL
