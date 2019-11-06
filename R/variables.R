#' FinBIF record variables
#'
#' FinBIF record variables that can be selected in a finbif occurrence search.
#'
#' @section Identifiers:
#' All identifiers are returned in the form of a URI. Identifiers include:
#'
#' - `record_id` Character.
#'   The ID of a record of organism's occurrence at a time and
#'   place.
#' - `individual_id` Character.
#'   ID of an individual organism (e.g., a ringed bird that has been captured
#'   multiple times will have a single `individual_id` and multiple `record_id`s
#'   corresponding to each capture).
#' - `event_id` Character.
#'   Event ID. An event can contain one or more records (e.g., a survey of
#'   plants at a particular location and time.)
#' - `document_id` Character.
#'   Document ID. A set of events that share common metadata.
#' - `form_id` Character.
#'   Form ID. The form used to create the document, event, record data.
#' - `collection_id` Character.
#'   Collection ID. All documents, events, and records belong to a collection
#'   (e.g., a museum collection, or the datasets collected by a specific
#'   institution). Collections themselves can be part of a larger
#'   (super)collection (e.g., all the collections at a specific museum). Only
#'   the lowest level collection ID for a record is returned. Use
#'   `finbif_collections()` to explore the hierarchy of collections.
#' - `source_id` Character.
#'   Source ID. The source of the collection's data.
#'
#' @section Taxa:
#' Variables related to taxonomy of records include:
#'
#' - `taxon_id` Character.
#'   The taxon ID in the form of a URI.
#' - `orig_taxon_id` Character.
#'   The taxon ID before (if any) annotation.
#' - `annotated_taxon_id` Character.
#'   The new taxon ID if the record has had it's taxonomy annotated.
#' - `reported_taxon_id` Character.
#'   The taxon ID as originally reported by the record creator.
#' - `scientific_name` Character.
#'   Scientific name of taxon.
#' - `orig_scientific_name` Character.
#'   The scientific name before (if any) annotation.
#' - `scientific_display_name` Character.
#'   Scientific name of taxon formatted for display (e.g., taxa with genus only
#'   will be formatted as _Genus sp._).
#' - `orig_scientific_display_name` Character.
#'   Scientific display name before (if any) annotation.
#' - `common_name` Character.
#'   Common name of taxon.
#' - `orig_common_name` Character.
#'   Common name before (if any) annotation.
#' - `reported_name` Character.
#'   The name of the taxon as originally reported by the record creator.
#' - `scientific_name_italicised` Logical.
#'   Is the scientific name normally italicised (i.e., is the taxonomic rank
#'   genus or below.)
#' - `orig_scientific_name_italicised` Logical.
#'   Is the original scientific name normally italicised.
#' - `scientific_name_author` Character.
#'   The authority for the taxon scientific name.
#' - `orig_scientific_name_author` Character.
#'   The authority for the taxon scientific name before (if any) annotation.
#' - `reported_author` Character.
#'   The authority of the taxon as originally reported by the record creator.
#' - `taxon_rank` Character.
#'   The taxonomic rank of the taxon (in the form of a URI).
#' - `orig_taxon_rank` Character.
#'   The taxonomic rank of the taxon (in the form of a URI) before (if any)
#'   annotation.
#' - `informal_groups` Character vector.
#'   The informal taxonomic groups that the taxon belongs to (e.g., birds) in
#'   the form of URIs.
#' - `orig_informal_groups` Character vector.
#'   The informal taxonomic groups that the taxon belonged to before (if any)
#'   annotation.
#' - `reported_informal_groups` Character vector.
#'   The informal taxonomic groups that the taxon belongs to as reported by the
#'   record creator.
#' - `taxon_checklist` Character.
#'   The checklist (as a URI) that that taxon is found in.
#' - `orig_taxon_checklist` Character.
#'   The checklist (as a URI) that that taxon was found in before (if any)
#'   annotation.
#' - `taxon_finnish` Logical.
#'   Is the taxon considered Finnish. The definition of a Finnish taxon differs
#'   by taxonomic group?
#' - `orig_taxon_finnish` Logical.
#'   Was the taxon considered Finnish before (if any) annotation?
#' - `is_unidentifiable` Logical.
#'   Is the record unable to be identified?
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
#' - `{female|male}_abundance` Integer.
#'   Number of female or male individuals recorded.
#' - `pair_abundance` Integer.
#'   Number of mating pairs recorded.
#' - `abundance_verbatim` Character.
#'   The abundance as reported by the record creator.
#' - `life_stage` Character.
#'   Life stage of individual(s) recorded.
#' - `sex` Character.
#'   Sex of individual(s) recorded.
#'
#' @section Location:
#' Variables related to the location of records include:
#'
#' - `{lat|lon}_wgs84` Numeric.
#'   Coordinates (in [WGS84](https://spatialreference.org/ref/epsg/wgs-84/)
#'   coordinate system) of the central point of a bounding box encompassing the
#'   record's geographic coverage.
#' - `{lat|lon}_{min|max}_{euref|kkj|wgs84}`. Numeric.
#'   Vertices of a bounding box encompassing the record's geographic coverage.
#'   Coordinates are available in
#'   [EUREF](https://spatialreference.org/ref/epsg/etrs89-etrs-tm35fin/),
#'   [KKJ](https://spatialreference.org/ref/epsg/2393/), or
#'   [WGS84](https://spatialreference.org/ref/epsg/wgs-84/).
#' - `coordinates_uncertainty` Integer.
#'   The horizontal distance (in meters) from the record's given coordinates
#'   describing the smallest circle containing the whole of the record's
#'   location.
#' - `coordinates_source`
#' - `footprint_{euref|kkj|wgs84}` Character.
#'   Well-Known Text (WKT) representation of the geographic shape defining the
#'   location of the record in either EUREF, KKJ or WGS84 coordinate systems.
#' - `country` Character.
#'   The country of the record's location.
#' - `province` Character.
#'   The administrative area directly below the level of country. For data from
#'   Finland FinBIF uses the concept of
#'   [Biogeographical Province](https://laji.fi/en/theme/emk). See link for
#'   details.
#' - `municipality`. Character.
#'   Administrative level below province.
#' - `higher_geography` Character.
#'   Geographic place name that is at higher level than country.
#' - `line_length_m` Integer.
#'   The length of linear locations (e.g., line transect surveys).
#' - `area_m2`
#'   The size of record's location.
#' - `is_breeding_location` Logical.
#'   Whether or not the occurrence is recorded at a known breeding location.
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
#' - `observers_ids`
#' - `determiner`
#' - `record_notes`
#' - `record_basis`
#' - `reference_publication`
#' - `superrecord_basis`
#' - `type_specimen`
#' - `record_order`
#' - `wild_status`
#' - `license`
#' - `document_notes`
#' - `partial`
#' - `event_order`
#' - `event_notes`
#' - `taxon_cenus_id`
#' - `taxon_census_type`
#' - `record_annotation_count`
#' - `{document|record}_keywords`
#' - `sample_count`
#'
#' @name variables
NULL
