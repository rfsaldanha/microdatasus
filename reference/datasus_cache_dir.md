# Cache directory used by microdatasus

Returns a platform-appropriate directory that users may pass to
`cache_dir` or set in `options(microdatasus.cache_dir = ...)`. Calling
this function, even with `create = TRUE`, does not change that option or
enable persistent caching for subsequent downloads or processing calls.

## Usage

``` r
datasus_cache_dir(create = FALSE)
```

## Arguments

- create:

  Logical scalar. If `TRUE`, create the directory.

## Value

A normalized directory path.

## Examples

``` r
cache <- datasus_cache_dir()
# To enable persistent caching for subsequent calls:
# options(microdatasus.cache_dir = cache)
```
