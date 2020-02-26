# finbif 0.2.0.9005

### MINOR IMPROVEMENTS

  * Global option to set timezone, "finbif_tz", as default value for "tzone"
    argument to finbif_occurrence(). Avoids having to set system environment
    variable TZ or specify "tzone" every time finbif_occurrence() is run.

# finbif 0.2.0.9004

### MINOR IMPROVEMENTS

  * Retired "quiet" option for "on_check_fail" argument in function
    `finbif_occurrence()`.
  * Improved error messages when taxa fail checking in `finbif_occurrence()`.

### BUG FIXES

  * Fixed bug in handling of duplicates that could result in an infinite
    recursion.
  * Fixed bug that (when "on_check_fail" = "warn") all taxa failed checks
    `finbif_occurrence()` would proceed as if no taxa had been selected.

# finbif 0.2.0.9003

### NEW FEATURES

  * All user facing functions with a `finbif_` prefix can now also be used with
    the alternative shorter prefix `fb_`.
  * Crop Wild Relative, CWR, added to admin statuses.

### MINOR IMPROVEMENTS

  * Front matter of vignettes is now visible when using the R help browser.

# finbif 0.2.0.9002

### BUG FIXES

  * Ordering by descending variables did not work when ordering by both
    ascending and descending variables.

# finbif 0.2.0.9001

### MINOR IMPROVEMENTS

  * Caching defaults to in memory caching instead of relying on the temporary
    directory.

# finbif 0.2.0

### NEW FEATURES

  * Add capacity to request a random sample of FinBIF records.

### MINOR IMPROVEMENTS

  * Add more content to vignettes.

# finbif 0.1.0

  * Initial release.
