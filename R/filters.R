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
#' - `informal_group_reported` Character vector.
#'   Filter by informal taxonomic groups including groups reported directly with
#'   the record and those linked to the recorded taxa in the FinBIF database.
#'   Use the function [finbif_informal_groups()] to see the informal taxonomic
#'   groups available in FinBIF.
#' - `administrative_status` Character vector.
#'   Filter by administrative status code. Use the function
#'   [finbif_admin_status()] to see administrative statuses and codes.
#' - `red_list_status` Character vector.
#'   Filter by IUCN red list status code. Use the function [finbif_red_list()]
#'   to see red list statuses and codes.
#' - `primary_habitat` Character vector or named list of character vectors.
#'   Filter by primary habitat code. Use the function [finbif_habitat_types()]
#'   to see habitat (sub)types and codes for taxa in the FinBIF database.
#'   Habitat type/subtypes can be refined further by indicating habitat
#'   qualifiers with a named list of character vectors where the names are
#'   habitat (sub)type codes and the elements of the character vector are the
#'   habitat qualifier codes. Use the function [finbif_habitat_qualifiers()] to
#'   see habitat qualifiers and codes. The records returned will be of taxa
#'   whose primary habitat is considered to be the (sub)habitat/habitat
#'   qualifier combination supplied.
#' - `primary_secondary_habitat` Character or named list of character vectors.
#'   As above, except the records returned will be of taxa whose primary or
#'   secondary habitat is considered to be the combination supplied.
#' - `finnish_occurrence_status`, Character vector.
#'   Filter by Finnish occurrence status of taxa. Use
#'   [finbif_finnish_occurrence()] to see the possible occurrence statuses of
#'   taxa.
#' - `finnish_occurrence_status_neg`, Character vector.
#'   Negation of the above. Selecting a status will filter out rather than
#'   include records with the selected status.
#' - `finnish`. Logical.
#'   If `TRUE`, limit records to taxa thought to occur in Finland. Or if `FALSE`
#'   limit to taxa not thought to occur in Finland. If unspecified (default)
#'   return records of all taxa.
#' - `invasive`. Logical.
#'   If `TRUE`, limit records to invasive taxa. Or if `FALSE` limit to
#'   non-invasive taxa. If unspecified (default) return records of invasive and
#'   non-invasive taxa.
#' - `taxon_rank`. Character vector.
#'   Filter by taxonomic rank. Use `finbif_taxon_rank()` to see the taxonomic
#'   ranks available. Records returned will be limited to the specified ranks
#'   and not include records of lower taxonomic levels.
#'
#' @section Location:
#' Filters related to location of record include:
#'
#' - `locality` Character vector.
#'   Filter by name of locality. Will first try to match strings to the
#'   countries, provinces, and municipalities (see below) in FinBIF if none of
#'   these locality types match exactly then will return records with verbatim
#'   locality matches in the original records.
#' - `country` Character vector.
#'   Filter by country. Use `finbif_countries()` to see country names and ISO
#'   codes (2 and 3 character) used in FinBIF.
#' - `province` Character vector.
#'   Filter by province. Use `finbif_provinces()` to see province names and
#'   codes.
#' - `municipality` Character vector.
#'   Filter by country. Use `finbif_municipalities()` to see municipality names.
#' - `bird_assoc_area` Character vector.
#'   Filter by BirdLife Finland association area. Use `finbif_bird_assoc_area()`
#'   to see association names and codes.
#' - `coordinates_area` Coordinates.
#'   A character vector or list of coordinate data. Must be length 3 to 4 (e.g.,
#'   `list(lat = c(60.4, 61), lon = c(22, 22.5), system = "wgs84", ratio = 1)`.
#'   The first element is minimum and maximum latitude and the second minimum
#'   and maximum longitude (or can be minimums only). The third element is the
#'   coordinate system; either one of `"wgs84"`, `"euref"` or `"kkj"`. The
#'   optional fourth element is a positive value less than `1`. When `1`, the
#'   coverage area of the returned records will be completely within the box
#'   bound by the coordinates values. Values less than `1` requires the returned
#'   record's coverage to overlap with the bounding box in that proportion. When
#'   using the system "`kkj`" the coordinates will be coerced to integers with
#'   units inferred from the number of integer digits (7 digits equals km's, 6
#'   equals 10km's, etc.,). If coordinate maximums are not specified they will
#'   be assumed to be one unit above the minimums (e.g., `c(666, 333, "kkj")` is
#'   equivalent to `list(c(6660000, 6670000), c(3330000, 3340000), "kkj")`).
#' - `coordinates_center` Coordinates.
#'   A character vector or list of coordinate data. Must be of length 3. The
#'   first two elements are latitude and longitude and third is the coordinate
#'   system (currently only `"wgs84"` is implemented). Records returned will be
#'   those for which the center point exactly matches that which is specified.
#' - `coordinates_cell_*k` Coordinates.
#'   A vector of coordinate data (lat, long). Filter by grid cell at scale `*`.
#'   Where `*` is 1, 10, 50 or 100. The coordinates specify the southeast corner
#'   of the cell. Coordinates system is uniform `"kkj"` (also known as "ykj").
#' - `coordinates_cell_*k_center` Coordinates.
#'   As above, except coordinates indicate center of grid cell.
#' - `coordinate_source` Character.
#'   Filter by source of coordinates. Currently accepted values are
#'   `"reported_value"` (coordinates were recorded at time of observation) and
#'   `"finnish_municipality"` (coordinates were derived and observer only
#'   recorded municipality).
#'
#' @section Time:
#' Filters related to time of record include:
#'
#' - `date_range_ymd` Dates.
#'   An \link[lubridate:Interval-class]{Interval} object or a vector of one to
#'   two \link[base:Dates]{Date} objects (start and end dates) or objects that
#'   are coercible to the \link[base:Dates]{Date} class by
#'   \link[lubridate]{as_date}. When supplying dates as strings, the day or
#'   month-and-day can be omitted (e.g.,`"2001-04"` or `"2001"`). Note however,
#'   that when omitting day, only "`-`" is allowed to separate year and month,
#'   and months must in two-digit/leading zero form. If the start or end dates
#'   are partial date strings they will be interpreted as the first or last day
#'   of the month or year (e.g., `c(2001, 2003)` is equivalent to
#'   `c("2001-01-01", "2003-12-12")`). If a single date is supplied as a partial
#'   date string then all records that fall within that month or year will be
#'   returned (e.g., `c("2001-01")` is equivalent to
#'   `c("2001-01-01", "2001-01-31")`).
#' - `date_range_ym` Dates.
#'   As above, but days (if supplied) will be ignored.
#' - `date_range_d` Integer vector.
#'   Filter by day of the year (e.g., `1` to `366`). If start or end date is
#'   omitted then it is interpreted as the first or last or end day of the year.
#' - `date_range_md` Character vector.
#'   Filter by month and day of the year (e.g., `"01-01"` to `"12-31"`). If
#'   start or end date is omitted then it is interpreted as the first or last or
#'   end day of the year.
#' - `{first|last}_import_date_{min|max}` Date.
#'   Filter by date record was imported. A \link[base:Dates]{Date} object or
#'   object that is coercible to the \link[base:Dates]{Date} class by
#'   \link[lubridate]{as_date}.
#'
#' @section Quality:
#' Filters related to quality of record:
#'
#' - `collection_reliability` Integer.
#'   Filter by collection reliability rating, from low reliability (1) to highly
#'   reliable (5).
#' - `coordinate_accuracy_max` Integer.
#'   Filter by maximum accuracy of coordinates (i.e.,
#'   `coordinate_accuracy_max = 100` will return records that are accurate to
#'   100m).
#' - `quality_issues`. Character.
#'   Filter by the presence of record quality issues. One of `"without_issues"`,
#'   `"with_issues"` or `"both"`. Issues include any quality issues with the
#'   record, the event, or the document. The default is `"without_issues"`
#'   unless filtering by record, event or document ID or record annotation
#'   status.
#' - `reliable` Logical.
#'   Filter by reliability of record. The `"collection_reliability"` is 4-5 or
#'   the record has been "confirmed" and has no quality issues (record, event or
#'   document).
#' - `taxon_reliability` Character vector.
#'   Filter by reliability of taxon identification. Can be one or more of
#'   `"reliable"`, `"likely"`, `"neutral"`, `"unlikely"` or `"unreliable"`.
#'
#' @section Misc:
#' Other filters:
#'
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
