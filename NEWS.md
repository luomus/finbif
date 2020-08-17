# finbif 0.3.0.9000

#### NEW FEATURES

  - New administrative statuses added to filters.

#### MINOR IMPROVEMENTS

  - Filtering vignette updated to reflect changes to data quality
    filters.

# finbif 0.3.0

#### NEW FEATURES

  - All user facing functions with a `finbif_` prefix can now also be
    used with the alternative shorter prefix `fb_`.

  - Crop Wild Relative, CWR, added to admin statuses.

  - There are two new utility functions, `to_dwc` and `to_native` for
    converting variable names to and from Darwin Core style.

  - Users can select and order by variables in Darwin Core style when
    using `finbif_occurrence`.

  - Some changes in variables and filters have flowed from upstream
    changes to "api.laji.fi". The variables `is_unidentifiable`,
    `record_reliable`, `collection_reliability`, `taxon_reliability`,
    `taxon_reliability_message` and `taxon_reliability_source` have been
    deprecated and replaced with `requires_verification`,
    `requires_identification`, `record_reliability` and
    `record_quality`. The filters `collection_reliability` and
    `taxon_reliability` have been deprecated and replaced with
    `requires_verification`, `collection_quality`, `record_reliability`,
    `record_quality`, and `expert_verified`.

  - Vernacular names are now localised. Users can select a language to
    use for taxon vernacular names. Missing names will fallback
    gracefully to other languages. A package-wide locale can be set and
    is by default set to the system locale (if not set or can't be
    determined it will default to English).

  - New vignettes on getting occurrence records, selecting and ordering
    variables, metadata and plotting have been added.

#### MINOR IMPROVEMENTS

  - Caching defaults to in memory caching instead of relying on the
    temporary directory.

  - Front matter of vignettes is now visible when using the R help
    browser.

  - Retired "quiet" option for "on\_check\_fail" argument in function
    `finbif_occurrence()`.

  - Improved error messages when taxa fail checking in
    `finbif_occurrence()`.

  - Global option to set timezone, "finbif\_tz", as default value for
    "tzone" argument to `finbif_occurrence()`. Avoids having to set
    system environment variable TZ or specify "tzone" every time
    `finbif_occurrence()` is run.

  - Now when a record has no time information the start time is assumed
    to be midday. Previous behaviour was to assume start time was
    midnight, making errors potentially biased.

  - Package options are now documented in the package level man page.

  - News file is now accessible via R internal help system.

  - The number of default variables selected when accessing occurrence
    records has been reduced to speed up downloads and improve the
    display of `finbif_occ` objects.

  - The print method for occurrence record objects has been updated. It
    is now aware of console width and when truncating variable values is
    more considerate of the context.

  - Variables can now be "deselected" when using the `select` argument
    to `finbif_occurrence` by prepending the variable name with a "`-`".

  - After some failures, API requests are now automatically retried up
    to three times.

#### BUG FIXES

  - Ordering by descending variables did not work when ordering by both
    ascending and descending variables.

  - Fixed bug in handling of duplicates that could result in an infinite
    recursion.

  - Fixed bug that (when "on\_check\_fail" = "warn") all taxa failed
    checks 'finbif\_occurrence()' would proceed as if no taxa had been
    selected.

  - Fixed bug in print method for `finbif_occ` objects that caused error
    when trying to display a single column objects with a list-col only.

# finbif 0.2.0

#### NEW FEATURES

  - Add capacity to request a random sample of FinBIF records.

#### MINOR IMPROVEMENTS

  - Add more content to vignettes.

# finbif 0.1.0

  - Initial release.
