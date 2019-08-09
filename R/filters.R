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
#'   If `TRUE` (default) return records of all taxa belonging to specified taxa.
#'   Or, if `FALSE` only return records for exact matches to the specified taxa
#'   (e.g., if a genus is specified, do not return records of the species
#'   belonging to the genus, return records of individuals identified as that
#'   genus only and not identified to a lower taxonomic level).
#' - `invalid_taxa` Logical.
#'   If `TRUE` (default) return records for taxa not found in the FinBIF
#'   taxonomic database as well as taxa that are in the FinBIF database. Or, if
#'   `FALSE` limit records to only those of taxa found in the FinBIF database.
#' - `informal_group` Character vector.
#'   Filter by informal taxonomic groups. Only including informal groups linked
#'   to the recorded taxa in the FinBIF database. Use the function
#'   [finbif_informal_groups()] to see the informal taxonomic groups available
#'   in FinBIF.
#' - `informal_group_reported` Character vector. Filter by informal taxonomic
#'   groups including groups reported directly with the record and those linked
#'   to the recorded taxa in the FinBIF database. Use the function
#'   [finbif_informal_groups()] to see the informal taxonomic groups available
#'   in FinBIF.
#' - `administrative_status` Character vector. Filter by administrative status
#'   code. Use the function [finbif_admin_status()] to see administrative
#'   statuses and codes.
#' - `red_list_status` Character vector. Filter by IUCN red list status code.
#'   Use the function [finbif_red_list()] to see red list statuses and codes.
#' - `primary_habitat` Character or named list of character vectors. Filter by
#'   primary habitat code. Use the function [finbif_habitat_types()] to see
#'   habitat (sub)types and codes for taxa in the FinBIF database. Habitat
#'   type/subtypes can be refined further by indicating habitat qualifiers with
#'   a named list of character vectors where the names are habitat (sub)type
#'   codes and the elements of the character vector are the habitat qualifier
#'   codes. Use the function [finbif_habitat_qualifiers()] to see habitat
#'   qualifiers and codes. The records returned will be of taxa whose primary
#'   habitat is considered to be the (sub)habitat/habitat qualifier combination
#'   supplied.
#' - `primary_secondary_habitat` Character or named list of character vectors.
#'   As above, except the records returned will be of taxa whose primary or
#'   secondary habitat is considered to be the combination supplied.
#' - `finnish`. Logical. If `TRUE`, limit records to taxa native to Finland. Or
#'   if `FALSE` limit to taxa not native to Finland. If unspecified (default)
#'   return records of taxa that are native and non-native to Finland.
#' - `invasive`. Logical. If `TRUE`, limit records to invasive taxa. Or if
#'   `FALSE` limit to non-invasive taxa. If unspecified (default) return records
#'   of invasive and non-invasive taxa.
#' - `taxon_rank`. Character vector. Filter by taxonomic rank. Use
#'   `finbif_taxon_rank()` to see the taxonomic ranks available. Records
#'   returned will be limited to the specified ranks and not include records of
#'   lower taxonomic levels.
#'
#' @section Location:
#' Filters related to location of record include:
#'
#' - `country` Character vector. Filter by country. Use `finbif_countries()` to
#'   see country names and ISO codes (2 and 3 character) used in FinBIF.
#' - `province` Character vector. Filter by province. Use `finbif_provinces()`
#'   to see province names and codes.
#' - `municipality` Character vector. Filter by country. Use
#'   `finbif_municipalities()` to see municipality names.
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
