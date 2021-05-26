#' FinBIF record variables
#'
#' FinBIF record variables that can be selected in a finbif occurrence search.
#'
#' @section Identifiers:
#' All identifiers are returned in the form of a URI. Identifiers include:
#'
#' - `record_id` Character.
#'   The ID of a record of organism's occurrence at a time and place.
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
#'   Common (vernacular) name of taxon.
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
#' - `informal_groups` List.
#'   The informal taxonomic groups that the taxon belongs to (e.g., birds) in
#'   the form of URIs.
#' - `orig_informal_groups` List.
#'   The informal taxonomic groups that the taxon belonged to before (if any)
#'   annotation.
#' - `reported_informal_groups` List.
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
#' - `{lat|lon}_{euref|wgs84}` Numeric.
#'   Coordinates (in
#'   [EUREF](https://spatialreference.org/ref/epsg/etrs89-etrs-tm35fin/) or
#'   [WGS84](https://spatialreference.org/ref/epsg/wgs-84/) coordinate system)
#'   of the central point of a bounding box encompassing the record's geographic
#'   coverage.
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
#' - `coordinates_source` Character.
#'   Source of coordinates.
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
#' - `area_m2` Integer.
#'   The size of record's location in meters squared.
#' - `is_breeding_location` Logical.
#'   Whether or not the occurrence is recorded at a known breeding location.
#'
#' @section Time:
#' Variables related to time of record include:
#'
#' - `date_time` POSIXct.
#'    The date and time of the recording event. This variable is computed after
#'    records are downloaded from FinBIF. Its timezone and accuracy can be
#'    controlled see `finbif_occurrence()` for details.
#' - `duration` Duration.
#'   The duration of the recording event. This variable is computed after
#'   records are downloaded from FinBIF.
#' - `date_start` Character.
#'   The date the recording event began.
#' - `date_end` Character.
#'   The date the recording event ended.
#' - `hour_start` Integer.
#'   The hour (24 hour time) of the day the recording event began.
#' - `hour_end` Integer.
#'   The hour (24 hour time) of the day the recording event ended.
#' - `minute_start` Integer.
#'   The minute of the hour the recording event began.
#' - `minute_end` Integer.
#'   The minute of the hour the recording event started.
#' - `ordinal_day_start` Integer.
#'   The ordinal day of the year the recording event began.
#' - `ordinal_day_end` Integer.
#'   The ordinal day of the year the recording event ended
#' - `season_start` Integer.
#'   The day of the year the recording event began. A four digit number
#'   indicating the day of the year in MMDD (%m%d) format.
#' - `season_end` Integer.
#'   The day of the year the recording event ended. A four digit number
#'   indicating the day of the year in MMDD (%m%d) format.
#' - `century` Integer.
#'   The century during which the recording event occurred (`NA` if the event
#'   spans multiple centuries).
#' - `decade` Integer.
#'   The decade during which the  recording event occurred (`NA` if the event
#'   spans multiple decades).
#' - `year` Integer.
#'   The year during which the recording event occurred (`NA` if the event spans
#'   multiple years).
#' - `month` Integer.
#'   The month of the year during which the recording event occurred (`NA` if
#'   the event spans multiple months).
#' - `day` Integer.
#'   The day of the month during which the recording event occurred (`NA` if the
#'   event spans multiple days).
#' - `formatted_date_time` Character.
#'   Date and time of the recording event formatted for display.
#' - `date_created` Character.
#'   The date the original data was created.
#' - `first_load_date` Character.
#'   The date the record was first loaded into the FinBIF database.
#' - `modified_date` Character.
#'   The most recent date the original data was modified.
#' - `load_date` Character.
#'   The most recent date the record was loaded into the FinBIF database.
#'
#' @section Data restrictions:
#' Variables related to restricted records include:
#'
#' - `restriction` Logical.
#'   Has the record been restricted in some way (e.g., geospatially aggregated).
#' - `restriction_level` Character.
#'   What level of restriction has been applied to the record.
#' - `restriction_reasons` List.
#'   List of reasons restriction has been applied.
#'
#' @section Data quality:
#' Variables related to the quality of records include:
#'
#' - `any_issues` Logical.
#'   Are there any data quality issues associated with the record, its event or
#'   document.
#' - `reported_taxon_confidence`
#'    Reliability of the record's taxonomic identification as reported by the
#'    original data author.
#' - `{document|time|location|event|record}_issue` Character.
#'   Issues with record associated with its document, time, location, event, or
#'   the record itself.
#' - `{document|time|location|event|record}_issue_message` Character.
#'   Details about the issue.
#' - `{document|time|location|event|record}_issue_source` Character.
#'   Source determining the issue.
#' - `requires_verification` Logical.
#'   Has the record been flagged for expert verification?
#' - `requires_identification` Logical.
#'   Has the record been flagged for expert identification?
#' - `record_reliability` Character.
#'   Indication of the record's reliability.
#' - `record_quality` Character.
#'   Indication of the record's quality.
#'
#' @section Misc:
#' Other variables:
#' - `collection` Character.
#'   Collection name. All documents, events, and records belong to a collection
#'   (e.g., a museum collection, or the datasets collected by a specific
#'   institution). Collections themselves can be part of a larger
#'   (super)collection (e.g., all the collections at a specific museum). Only
#'   the lowest level collection name for a record is returned. Use
#'   `finbif_collections()` to explore the hierarchy of collections.
#' - `observers_ids` List.
#'   List of observer identifiers for the record.
#' - `determiner` Character.
#'   Person who determined the taxonomic identification of the record.
#' - `record_basis` Character.
#'   The type of or method used to obtain the record.
#' - `superrecord_basis` Character.
#'   Higher level type of or method used to obtain the record.
#' - `type_specimen` Logical.
#'   Whether or not the record is of a type specimen.
#' - `is_wild` Logical.
#'   Whether or not the record is of a "wild" organism.
#' - `license` Character.
#'   The license of the data associated with the record.
#' - `{document|event|record}_notes` Character.
#'   Notes associated with the document, event or record itself.
#' - `{document|record}_keywords` List.
#'   List of keywords associated with the document or record.
#' - `record_annotation_count` Integer.
#'   How many annotations are associated with the record.
#' - `sample_count` Integer.
#'   How many material samples (DNA extractions, etc., ...) are associated with
#'   the record.
#' - `{document|event|record}_media_count` Integer.
#'   How many media items (images, audio, video, etc., ...) are associated with
#'   the record's document, event or the record itself.
#'
#' @name variables
NULL
