#' Filtering FinBIF records
#'
#' Filters available for FinBIF records and occurrence data.
#'
#' @section Taxa:
#' Filters related to taxa include:
#'
#' - `taxon_id` Character vector.
#'   FinBIF taxon IDs. The functions [finbif_check_taxa()] and [finbif_taxa()]
#'   can be used to search for taxon IDs.
#' - `taxon_name` Character vector.
#'   Filter based on taxon names (scientific or common) rather than IDs. If the
#'   specified taxa are not found in the FinBIF taxonomy then matches are
#'   attempted with the occurrence record names as originally supplied verbatim.
#' - `quality_controlled_det` Logical.
#'   If `TRUE` (default) use quality controlled taxonomic determinations. Or, if
#'   `FALSE` use the originally recorded taxonomic determinations.
#' - `subtaxa` Logical.
#'   If `TRUE` (default) return records of all taxa belonging to the specified
#'   taxon. Or, if `FALSE` only return records for exact matches to the
#'   specified taxon (e.g., if a genus is specified do not return records of the
#'   species belonging to genus only return records with individuals identified
#'   as that genus only).
#' - `invalid_taxa` Logical.
#'   If `TRUE` (default) return records that have taxa found in the FinBIF
#'   taxonomic database. Or, if `FALSE` limit records to only those with taxa
#'   found in the FinBIF database.
#' - `informal_group` Character vector.
#'   Filter by informal taxonomic groups. Only including informal groups linked
#'   to the recorded taxa in the FinBIF database. Use the function
#'   [finbif_informal_groups()] to see the informal taxonomic groups available
#'   in FinBIF.
#' - `informal_group_reported` Character. Filter by informal taxonomic groups
#'   including groups reported directly with the record and those linked
#'   to the recorded taxa in the FinBIF database. Use the function
#'   [finbif_informal_groups()] to see the informal taxonomic groups available
#'   in FinBIF.
#' - `administrative_status` Character. Filter by administrative status. Use the
#'   function [finbif_admin_status()] to see administrative statuses for taxa in
#'   the FinBIF database.
#' - `redlist_status` Character.
#' - `primary_habitat` Character.
#' - `primary_secondary_habitat` Character.
#' - `finnish`. Logical.
#' - `invasive`. Logical
#' - `taxon_rank`. Character.
#'
#' @section Location:
#' Filters related to location of record include:
#'
#' - `country` Character.
#' - `municipality` Character.
#' - `province` Character.
#' - `locality` Character.
#' - `bird_assoc_area` Character.
#' - `place_name_id` Character.
#' - `coordinates_area` Coordinates.
#' - `coordinates_center` Coordinates.
#' - `coordinates_cell_*k` Coordinates.
#' - `coordinates_cell_*k_center` Coordinates.
#' - `coordinate_source` Character.
#'
#' @section Time:
#' Filters related to time of record include:
#'
#' - `date_range_ymd` Date.
#' - `date_range_ym` Date.
#' - `date_range_d` Date.
#' - `date_range_md` Date.
#' - `last_import_date_min` Date.
#' - `last_import_date_max` Date.
#' - `first_import_date_min` Date.
#' - `first_import_date_max` Date.
#'
#' @section Quality:
#' Filters related to quality of record:
#'
#' - `collection_reliability` Integer.
#' - `coordinate_accuracy_max` Integer.
#' - `quality_issues`. Character.
#' - `reliable` Logical.
#' - `taxon_reliability` Character.
#'
#' @section Misc:
#' Other filters:
#'
#' - `occurrence_type`, Character.
#' - `not_occurrence_type` Character.
#' - `form_id` Character.
#' - `keywords` Character.
#' - `collection` Character.
#' - `not_collection` Character.
#' - `subcollections` Logical.
#' - `source` Character.
#' - `record_basis` Character.
#' - `super_record_basis` Character.
#' - `life_stage` Character.
#' - `sex` Character.
#' - `invasive_control` Character.
#' - `invasive_controlled` Logical.
#' - `event_id` Character.
#' - `document_id` Character.
#' - `record_id` Character.
#' - `individual_id` Character.
#' - `abundance_min` Integer.
#' - `abundance_max` Integer.
#' - `type_specimen` Logical.
#' - `is_native` Logical.
#' - `wild_status` Character.
#' - `is_breeding_location` Logical.
#' - `has_document_media` Logical.
#' - `has_event_media` Logical.
#' - `has_record_media` Logical.
#' - `has_media` Logical.
#' - `editor_id` Character.
#' - `observer_id` Character.
#' - `editor_or_observer_id` Character.
#' - `event_observer_name` Character.
#' - `event_observer_id` Character.
#' - `secure_reason` Character.
#' - `secure_level` Character.
#' - `secure` Logical.
#' - `annotated` Logical.
#' - `unidentified` Logical.
#' - `taxon_census` Character.
#' - `record_fact` List.
#' - `event_fact` List.
#' - `document_fact` List.
#'
#' @name filters
NULL
