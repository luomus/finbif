# finbif 0.2.0.9008

#### MINOR IMPROVEMENTS

  - The print method for occurrence record objects has been updated. It
    is now aware of console width and when truncating variable values is
    more considerate of the context.

# finbif 0.2.0.9007

#### MINOR IMPROVEMENTS

  - Package options are now documented in the package level man page.

  - News file is now accessible via R internal help system.

  - The number of default variables selected when accessing occurrence
    records has been reduced to speed up downloads and improve the
    display of `finbif_occ` objects.

# finbif 0.2.0.9006

#### MINOR IMPROVEMENTS

  - Now when a record has no time information the start time is assumed
    to be midday. Previous behaviour was to assume start time was
    midnight, making errors potentially biased.

# finbif 0.2.0.9005

#### MINOR IMPROVEMENTS

  - Global option to set timezone, "finbif\_tz", as default value for
    "tzone" argument to finbif\_occurrence(). Avoids having to set
    system environment variable TZ or specify "tzone" every time
    finbif\_occurrence() is run.

# finbif 0.2.0.9004

#### MINOR IMPROVEMENTS

  - Retired "quiet" option for "on\_check\_fail" argument in function
    'finbif\_occurrence()'.

  - Improved error messages when taxa fail checking in
    'finbif\_occurrence()'.

#### BUG FIXES

  - Fixed bug in handling of duplicates that could result in an infinite
    recursion.

  - Fixed bug that (when "on\_check\_fail" = "warn") all taxa failed
    checks 'finbif\_occurrence()' would proceed as if no taxa had been
    selected.

# finbif 0.2.0.9003

#### NEW FEATURES

  - All user facing functions with a `finbif_` prefix can now also be
    used with the alternative shorter prefix `fb_`.

  - Crop Wild Relative, CWR, added to admin statuses.

#### MINOR IMPROVEMENTS

  - Front matter of vignettes is now visible when using the R help
    browser.

# finbif 0.2.0.9002

#### BUG FIXES

  - Ordering by descending variables did not work when ordering by both
    ascending and descending variables.

# finbif 0.2.0.9001

#### MINOR IMPROVEMENTS

  - Caching defaults to in memory caching instead of relying on the
    temporary directory.

# finbif 0.2.0

#### NEW FEATURES

  - Add capacity to request a random sample of FinBIF records.

#### MINOR IMPROVEMENTS

  - Add more content to vignettes.

# finbif 0.1.0

  - Initial release.
