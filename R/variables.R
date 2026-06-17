#' FinBIF record variables
#'
#' FinBIF record variables that can be selected in a finbif occurrence search.
#'
#' @section Identifiers:
#' All identifiers are returned in the form of a URI. Identifiers include:
#'
#' - `occurrenceID` Character.
#'   The ID of a record of organism's occurrence at a time and place.
#' - `organismID` Character.
#'   ID of an individual organism (e.g., a ringed bird that has been captured
#'   multiple times will have a single `organismID` and multiple `occurrenceID`s
#'   corresponding to each capture).
#' - `eventID` Character.
#'   Event ID. An event can contain one or more records (e.g., a survey of
#'   plants at a particular location and time.)
#' - `catalogNumber` Character.
#'   Document ID. A set of events that share common metadata.
#' - `formID` Character.
#'   Form ID. The form used to create the document, event, record data.
#' - `datasetID` Character.
#'   Dataset ID. All documents, events, and records belong to a
#'   collection/dataset (e.g., a museum collection, or the datasets collected
#'   by a specific institution). Collections themselves can be part of a larger
#'   (super)collection (e.g., all the collections at a specific museum). Only
#'   the lowest level dataset ID for a record is returned. Use
#'   `finbif_collections()` to explore the hierarchy of collections/datasets.
#' - `sourceID` Character.
#'   Source ID. The source of the collection's data.
#'
#' @section Taxa:
#' Variables related to taxonomy of records include:
#'
#' - `taxonConceptID` Character.
#'   The taxon ID in the form of a URI.
#' - `originalTaxonConceptID` Character.
#'   The taxon ID before (if any) annotation.
#' - `annotatedTaxonConceptID` Character.
#'   The new taxon ID if the record has had it's taxonomy annotated.
#' - `verbatimTaxonConceptID` Character.
#'   The taxon ID as originally reported by the record creator.
#' - `scientificName` Character.
#'   Scientific name of taxon.
#' - `originalScientificName` Character.
#'   The scientific name before (if any) annotation.
#' - `scientificNameDisplay` Character.
#'   Scientific name of taxon formatted for display (e.g., taxa with genus only
#'   will be formatted as _Genus sp._).
#' - `originalScientificNameDisplay` Character.
#'   Scientific display name before (if any) annotation.
#' - `vernacularName` Character.
#'   Common (vernacular) name of taxon.
#' - `originalVernacularName` Character.
#'   Common name before (if any) annotation.
#' - `verbatimIdentification` Character.
#'   The name of the taxon as originally reported by the record creator.
#' - `scientificNameItalicised` Logical.
#'   Is the scientific name normally italicised (i.e., is the taxonomic rank
#'   genus or below.)
#' - `originalScientificNameItalicised` Logical.
#'   Is the original scientific name normally italicised.
#' - `scientificNameAuthorship` Character.
#'   The authority for the taxon scientific name.
#' - `originalScientificNameAuthorship` Character.
#'   The authority for the taxon scientific name before (if any) annotation.
#' - `verbatimScientificNameAuthorship` Character.
#'   The authority of the taxon as originally reported by the record creator.
#' - `taxonRank` Character.
#'   The taxonomic rank of the taxon.
#' - `originalTaxonRank` Character.
#'   The taxonomic rank of the taxon before (if any) annotation.
#' - `informalTaxonGroups` List.
#'   The informal taxonomic groups that the taxon belongs to (e.g., birds) in
#'   the form of URIs.
#' - `originalInformalTaxonGroups` List.
#'   The informal taxonomic groups that the taxon belonged to before (if any)
#'   annotation.
#' - `verbatimInformalTaxonGroups` List.
#'   The informal taxonomic groups that the taxon belongs to as reported by the
#'   record creator.
#' - `taxonChecklist` Character.
#'   The checklist (as a URI) that that taxon is found in.
#' - `originalTaxonChecklist` Character.
#'   The checklist (as a URI) that that taxon was found in before (if any)
#'   annotation.
#' - `taxonFinnishStatus` Logical.
#'   Is the taxon considered Finnish. The definition of a Finnish taxon differs
#'   by taxonomic group?
#' - `originalFinnishStatus` Logical.
#'   Was the taxon considered Finnish before (if any) annotation?
#'
#' @section Abundance, sex & life history:
#' Variables related to abundance, sex and life history include:
#'
#' - `individualCount` Integer.
#'   Number of individuals recorded.
#' - `individualCountInterpreted` Integer.
#'   Number of individuals recorded or inferred from the record. Note that many
#'   records with `individualCount == 1L` only indicate the record is of
#'   one individual and may not necessarily imply that this was the abundance at
#'   that specified place and time (e.g., a preserved museum specimen consisting
#'   of a single individual).
#' - `individualCount{Female|Male}` Integer.
#'   Number of female or male individuals recorded.
#' - `pairCount` Integer.
#'   Number of mating pairs recorded.
#' - `organismQuantity` Character.
#'   The abundance as reported by the record creator.
#' - `lifeStage` Character.
#'   Life stage of individual(s) recorded.
#' - `sex` Character.
#'   Sex of individual(s) recorded.
#'
#' @section Location:
#' Variables related to the location of records include:
#'
#' - `decimal{Latitude|Longitude}` Numeric.
#'   Coordinates (in
#'   [WGS84](https://spatialreference.org/ref/epsg/4326/) coordinate system)
#'   of the central point of a bounding box encompassing the record's geographic
#'   coverage.
#' - `decimal{Latitude|Longitude}EUREF` Numeric.
#'   Coordinates (in
#'   [EUREF](https://spatialreference.org/ref/epsg/3067/) coordinate system)
#'   of the central point of a bounding box encompassing the record's geographic
#'   coverage.
#' - `decimal{Latitude|Longitude}{Min|Max}{EUREF|YKJ|WGS84}`. Numeric.
#'   Vertices of a bounding box encompassing the record's geographic coverage.
#'   Coordinates are available in
#'   [EUREF](https://spatialreference.org/ref/epsg/3067/),
#'   [YKJ](https://spatialreference.org/ref/epsg/2393/), or
#'   [WGS84](https://spatialreference.org/ref/epsg/4326/).
#' - `coordinateUncertaintyInMeters` Integer.
#'   The horizontal distance (in meters) from the record's given coordinates
#'   describing the smallest circle containing the whole of the record's
#'   location.
#' - `georeferenceSources` Character.
#'   Source of coordinates.
#' - `footprintWKT` Character.
#'   Well-Known Text (WKT) representation of the geographic shape defining the
#'   location of the record in WGS84 coordinate systems.
#' - `footprintWKT{_EUREF|_YKJ}` Character.
#'   Well-Known Text (WKT) representation of the geographic shape defining the
#'   location of the record in either EUREF or YKJ coordinate systems.
#' - `country` Character.
#'   The country of the record's location.
#' - `stateProvince` Character.
#'   The administrative area directly below the level of country.
#' - `bioStateProvince` Character.
#'   For data from Finland FinBIF uses the concept of
#'   [Biogeographical Province](https://laji.fi/en/theme/emk). See link for
#'   details.
#' - `county`. Character.
#'   Administrative level below region
#' - `higherGeography` Character.
#'   Geographic place name that is at higher level than country.
#' - `lineLengthMeters` Integer.
#'   The length of linear locations (e.g., line transect surveys).
#' - `areaMetersSquared` Integer.
#'   The size of record's location in meters squared.
#' - `isBreedingSite` Logical.
#'   Whether or not the occurrence is recorded at a known breeding location.
#' - `locationID` Character.
#'   A location ID in the form of a URI.
#' - `eventSection` Integer.
#'   A numeric identifier for a sub-location of a location (e.g., a specific
#'   part of a transect that undergoes repeated surveys.)
#'
#' @section Time:
#' Variables related to time of record include:
#'
#' - `eventDateTime` POSIXct.
#'    The date and time of the recording event. This variable is computed after
#'    records are downloaded from FinBIF. Its timezone and accuracy can be
#'    controlled see `finbif_occurrence()` for details.
#' - `samplingEffort` Duration.
#'   The duration of the recording event. This variable is computed after
#'   records are downloaded from FinBIF.
#' - `eventDateStart` Character.
#'   The date the recording event began.
#' - `eventDateEnd` Character.
#'   The date the recording event ended.
#' - `hourStart` Integer.
#'   The hour (24 hour time) of the day the recording event began.
#' - `hourEnd` Integer.
#'   The hour (24 hour time) of the day the recording event ended.
#' - `minuteStart` Integer.
#'   The minute of the hour the recording event began.
#' - `minuteEnd` Integer.
#'   The minute of the hour the recording event started.
#' - `startDayOfYear` Integer.
#'   The ordinal day of the year the recording event began.
#' - `endDayOfYear` Integer.
#'   The ordinal day of the year the recording event ended
#' - `seasonStart` Integer.
#'   The day of the year the recording event began. A four digit number
#'   indicating the day of the year in MMDD (%m%d) format.
#' - `seasonEnd` Integer.
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
#' - `eventDateTimeDisplay` Character.
#'   Date and time of the recording event formatted for display.
#' - `documentCreatedDate` Character.
#'   The date the original data was created.
#' - `available` Character.
#'   The date the record was first loaded into the FinBIF database.
#' - `modified` Character.
#'   The most recent date the original data was modified.
#' - `documentLoadedDate` Character.
#'   The most recent date the record was loaded into the FinBIF database.
#'
#' @section Data restrictions:
#' Variables related to restricted records include:
#'
#' - `informationRestricted` Logical.
#'   Has the record been restricted in some way (e.g., geospatially aggregated).
#' - `dataGeneralizations` Character.
#'   What level of restriction has been applied to the record.
#' - `informationWithheld` List.
#'   List of reasons restriction has been applied.
#'
#' @section Data quality:
#' Variables related to the quality of records include:
#'
#' - `hasIssues` Logical.
#'   Are there any data quality issues associated with the record, its event or
#'   document.
#' - `identificationQualifer`
#'    Reliability of the record's taxonomic identification as reported by the
#'    original data author.
#' - `{document|time|location|event|occurrence}Issue` Character.
#'   Issues with record associated with its document, time, location, event, or
#'   the record itself.
#' - `{document|time|location|event|occurrence}IssueRemarks` Character.
#'   Details about the issue.
#' - `{document|time|location|event|occurrence}IssueSource` Character.
#'   Source determining the issue.
#' - `requiresVerification` Logical.
#'   Has the record been flagged for expert verification?
#' - `requiresIdentification` Logical.
#'   Has the record been flagged for expert identification?
#' - `occurrenceReliability` Character.
#'   Indication of the record's reliability.
#' - `identificationVerificationStatus` Character.
#'   Indication of the record's quality.
#'
#' @section Misc:
#' Other variables:
#' - `datasetName` Character.
#'   Dataset name. All documents, events, and records belong to a
#'   collection/dataset (e.g., a museum collection, or the datasets collected
#'   by a specific institution). Collections themselves can be part of a larger
#'   (super)collection (e.g., all the collections at a specific museum). Only
#'   the lowest level collection name for a record is returned. Use
#'   `finbif_collections()` to explore the hierarchy of collections/datasets.
#' - `recordedByIDs` List.
#'   List of observer identifiers for the record.
#' - `identifiedBy` Character.
#'   Person who determined the taxonomic identification of the record.
#' - `basisOfRecord` Character.
#'   The type of or method used to obtain the record.
#' - `basisOfSuperRecord` Character.
#'   Higher level type of or method used to obtain the record.
#' - `typeStatus` Logical.
#'   Whether or not the record is of a type specimen.
#' - `occurrenceIsWild` Logical.
#'   Whether or not the record is of a "wild" organism.
#' - `license` Character.
#'   The license of the data associated with the record.
#' - `{document|event|occurrence}Remarks` Character.
#'   Notes associated with the document, event or record itself.
#' - `{document|occurrence}Keywords` List.
#'   List of keywords associated with the document or record.
#' - `occurrenceAnnotationCount` Integer.
#'   How many annotations are associated with the record.
#' - `materialSampleCount` Integer.
#'   How many material samples (DNA extractions, etc., ...) are associated with
#'   the record.
#' - `{document|event|occurrence}AssociatedMediaCount` Integer.
#'   How many media items (images, audio, video, etc., ...) are associated with
#'   the record's document, event or the record itself.
#'
#' @name variables
NULL
