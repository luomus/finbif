# Caching FinBIF downloads

Working with cached data from FinBIF.

## Turning caching off

By default, local caching of most FinBIF API requests is turned on. Any
request made using the same arguments will only request data from FinBIF
in the first instance and subsequent requests will use the local cache
while it exists. This will increase the speed of repeated requests and
save bandwidth and computation for the FinBIF server. Caching can be
turned off temporarily by setting `cache = c(FALSE, FALSE)` in the
requesting function.

Setting
`options(finbif_use_cache = FALSE, finbif_use_cache_metadata = FALSE)`
will turn off caching for the current session.

## Using filesystem caching

By default cached requests are stored in memory. This can be changed by
setting the file path for the current session with
`options(finbif_cache_path = "path/to/cache")`.

## Using database caching

Caching can also be done using a database. Using a database for caching
requires the `DBI` package and a database backend package such as
`RSQLite` to be installed. To use the database for caching simply pass
the connection objected created with
[`DBI::dbConnect`](https://dbi.r-dbi.org/reference/dbConnect.html) to
the `finbif_cache_path` option (e.g.,
`db <- DBI::dbConnect(RSQLite::SQLite(), "my-db.sqlite"); `
`options(finbif_cache_path = db)` ).

## Timeouts

A cache timeout can be set by using an integer (number of hours until
cache is considered invalid and is cleared) instead of a logical value
for the `finbif_use_cache` and `finbif_use_cache_metadata` options or
the `cache` function arguments.

## Clearing the cache

The cache can be reset
[`finbif_clear_cache()`](https://finbif-docs-dev.netlify.app/reference/finbif_clear_cache.md).

## Updating the cache

The cache can be updated using
[`finbif_update_cache()`](https://finbif-docs-dev.netlify.app/reference/finbif_update_cache.md).
