# Detects the backend which will be used for checking the rules

The detection will be made based on the class of the object as well as
the packages installed. For example, if a `data.frame` is used, it will
look if `data.table` or `dplyr` are installed on the system, as they
provide more speed. Note the main functions will revert the

## Usage

``` r
detect_backend(x)
```

## Arguments

- x:

  The data object, ie a data.frame, tibble, data.table, arrow, or DBI
  object

## Value

a single character element with the name of the backend to use. One of
`base-r`, `data.table`, `dplyr`, `collectibles` (for arrow or DBI
objects)

## See also

[`check_data()`](https://davzim.github.io/dataverifyr/reference/check_data.md)

## Examples

``` r
data <- mtcars
detect_backend(data)
#> [1] "data.table"
```
