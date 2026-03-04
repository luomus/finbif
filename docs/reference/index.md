# Package index

## The finbif package

Package overview

- [`finbif`](https://luomus.github.io/finbif/reference/finbif-package.md)
  [`finbif-package`](https://luomus.github.io/finbif/reference/finbif-package.md)
  : finbif: Interface for the 'Finnish Biodiversity Information
  Facility' API

## Access token

Getting an access token

- [`finbif_request_token()`](https://luomus.github.io/finbif/reference/finbif_request_token.md)
  [`finbif_renew_token()`](https://luomus.github.io/finbif/reference/finbif_request_token.md)
  : Get a FinBIF personal access token

## Metadata

Exploring metadata

- [`finbif_metadata()`](https://luomus.github.io/finbif/reference/finbif_metadata.md)
  : FinBIF metadata
- [`finbif_collections()`](https://luomus.github.io/finbif/reference/finbif_collections.md)
  : FinBIF collections
- [`finbif_informal_groups()`](https://luomus.github.io/finbif/reference/finbif_informal_groups.md)
  : FinBIF informal groups

## Taxonomy

Getting information on taxa

- [`finbif_check_taxa()`](https://luomus.github.io/finbif/reference/finbif_check_taxa.md)
  : Check FinBIF taxa
- [`finbif_taxa()`](https://luomus.github.io/finbif/reference/finbif_taxa.md)
  [`common_name()`](https://luomus.github.io/finbif/reference/finbif_taxa.md)
  [`scientific_name()`](https://luomus.github.io/finbif/reference/finbif_taxa.md)
  [`taxon_id()`](https://luomus.github.io/finbif/reference/finbif_taxa.md)
  : Search the FinBIF taxa

## Records

Getting records from finbif

- [`finbif_last_mod()`](https://luomus.github.io/finbif/reference/finbif_last_mod.md)
  : Get last modified date for FinBIF occurrence records
- [`finbif_occurrence()`](https://luomus.github.io/finbif/reference/finbif_occurrence.md)
  : Download FinBIF occurrence records
- [`finbif_occurrence_load()`](https://luomus.github.io/finbif/reference/finbif_occurrence_load.md)
  : Load FinBIF occurrence records from a file
- [`filters`](https://luomus.github.io/finbif/reference/filters.md) :
  Filtering FinBIF records
- [`to_dwc()`](https://luomus.github.io/finbif/reference/to_dwc.md)
  [`to_native()`](https://luomus.github.io/finbif/reference/to_dwc.md)
  [`from_schema()`](https://luomus.github.io/finbif/reference/to_dwc.md)
  : Convert variable names
- [`variables`](https://luomus.github.io/finbif/reference/variables.md)
  : FinBIF record variables

## Cache

Caching finbif downloads

- [`caching`](https://luomus.github.io/finbif/reference/caching.md) :
  Caching FinBIF downloads
- [`finbif_clear_cache()`](https://luomus.github.io/finbif/reference/finbif_clear_cache.md)
  : Clear cache
- [`finbif_update_cache()`](https://luomus.github.io/finbif/reference/finbif_update_cache.md)
  : Update cache
