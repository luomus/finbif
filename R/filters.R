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
#'   [finbif_metadata()] to see administrative statuses and codes.
#' - `red_list_status` Character vector.
#'   Filter by IUCN red list status code. Use the function [finbif_metadata()]
#'   to see red list statuses and codes.
#' - `primary_habitat` Character vector or named list of character vectors.
#'   Filter by primary habitat code. Use the function [finbif_metadata()]
#'   to see habitat (sub)types and codes for taxa in the FinBIF database.
#'   Habitat type/subtypes can be refined further by indicating habitat
#'   qualifiers with a named list of character vectors where the names are
#'   habitat (sub)type codes and the elements of the character vector are the
#'   habitat qualifier codes. Use the function [finbif_metadata()] to
#'   see habitat qualifiers and codes. The records returned will be of taxa
#'   whose primary habitat is considered to be the (sub)habitat/habitat
#'   qualifier combination supplied.
#' - `primary_secondary_habitat` Character or named list of character vectors.
#'   As above, except the records returned will be of taxa whose primary or
#'   secondary habitat is considered to be the combination supplied.
#' - `finnish_occurrence_status`, Character vector.
#'   Filter by Finnish occurrence status of taxa. Use
#'   [finbif_metadata()] to see the possible occurrence
#'   statuses of taxa.
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
#'   Filter by municipality. Use `finbif_municipalities()` to see municipality
#'   names.
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
#' - `coordinates_cell_{1k|10k|50k|100k}` Coordinates.
#'   A vector of coordinate data (lat, long). Filter by grid cell at scale `*`.
#'   Where `*` is 1, 10, 50 or 100. The coordinates specify the southeast corner
#'   of the cell. Coordinates system is uniform `"kkj"` (also known as "ykj").
#' - `coordinates_cell_{1k|10k|50k|100k}_center` Coordinates.
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
#' - `keywords` Character vector.
#'   Filter by keywords.
#' - `collection` Character vector or `finbif_collections` data.frame.
#'   Filter by collection. If a character vector can refer to collection ID,
#'   collection name (in English, Finnish, or Swedish) or abbreviated name.
#'   Use `finbif_collections()` to see list of collections and metadata. Can
#'   also use the results of a call to `finbif_collections()` directly to
#'   filter records.
#' - `subcollections` Logical.
#'   If `TRUE` (default) include the subcollections of the collections
#'   specified. If `FALSE` do not include subcollections.
#' - `not_collection`  Character vector or `finbif_collections` data.frame.
#'   As for `collection`, but result will be the negation of the specified
#'   collections.
#' - `source` Character vector.
#'   Filter by information system data source. Use `finbif_sources()` to see
#'   data source IDs names and descriptions.
#' - `record_basis` Character vector.
#'   Filter by basis of record. Use `finbif_record_basis()` to see list of
#'   record bases.
#' - `superrecord_basis` Character vector.
#'   Filter by superset of record basis. One or more of `"human_observation"`,
#'   `"machine_observation"`, or `"specimen"`.
#' - `life_stage` Character vector.
#'   Filter by organism life stage. Use `finbif_life_stages()` to see list of
#'   organism life stages.
#' - `sex` Character vector.
#'   Filter by organism sex and sex-related category name or code. Use
#'   `finbif_sex_categories()` to see list of organism sexes and sex-related
#'   categories and codes. If `"male"` or `"female"` is specified then records
#'   returned will be those with sex specified as male or female respectively
#'   and those records where the corresponding `{male|female}_abundance > 1`.
#' - `event_id` Character.
#'   Filter by event (list of records, etc.) ID.
#' - `document_id` Character.
#'   Filter by the document (collection of events) ID of occurrences.
#' - `record_id` Character.
#' - `individual_id` Character.
#'   Filter by individual (an individual organism) ID.
#' - `abundance_min` Integer.
#'   Filter by the minimum number of individual organisms in the record.
#' - `abundance_max` Integer.
#'   Filter by the maximum number of individual organisms in the record.
#' - `type_specimen` Logical.
#'   Filter by whether or not the record is a type specimen.
#' - `wild_status` Character.
#'   Filter by "wildness" status of records. One or more of `"wild"`,
#'   `"non_wild"` or `"unknown"`. Default is `c("wild", "unknown")`.
#' - `is_breeding_location` Logical.
#'   Filter by whether or not the occurrence is recorded at a known breeding
#'   location.
#' - `has_document_media` Logical.
#'   Filter by whether there is media (images, video, audio, etc.) associated
#'   with the records' document.
#' - `has_event_media` Logical.
#'   Filter by whether there is media (images, video, audio, etc.) associated
#'   with the records' event.
#' - `has_record_media` Logical.
#'   Filter by whether there is media (images, video, audio, etc.) associated
#'   with the record.
#' - `has_media` Logical.
#'   Filter by whether there is any media (images, video, audio, etc.)
#'   associated with the record, its document or its event.
#' - `event_observer_name` Character.
#'   Filter by observer name.
#' - `event_observer_id` Integer.
#'   Filter by observer ID.
#' - `restriction_reason` Character vector.
#'   Filter by reason data has security restrictions. See
#'   `finbif_restriction_reasons()` for a list of reasons data may have security
#'   restrictions.
#' - `restriction_level` Character vector.
#'   Filter by data restriction level. See
#'   `finbif_restriction_levels()` for a list of the levels of data
#'   restrictions.
#' - `restricted` Logical.
#'   Filter records by whether any data restrictions are in place (`TRUE`) or
#'   not (`FALSE`).
#' - `annotated` Logical.
#'   Filter records that do (`TRUE`) or do not (`FALSE`) have annotations.
#' - `unidentified` Logical.
#'   Filter by whether the record has been identified to species level and
#'   linked to the FinBIF taxon database (`FALSE`) or has not been identified to
#'   species level reliably and linked to the taxon database (`TRUE`).
#' - `taxon_census` Character vector.
#'   Return records belonging to surveys or censuses of a given taxon or
#'   taxonomic group. Specify the taxonomic group with a FinBIF taxon ID. Use
#'   `finbif_check_taxa()` to find taxon IDs.
#' - `{record|event|document}_fact` Character vector.
#'   Filter by record, event or document facts. Facts are key-value pairs of the
#'   form `"<fact>=<value>"`. Value can be omitted in which case all records
#'   with any value recorded for the specified fact will be returned.
#' - `has_sample` Logical.
#'   Record includes a sample or samples (e.g., a DNA sample or preparation)
#'
#' @name filters
NULL
