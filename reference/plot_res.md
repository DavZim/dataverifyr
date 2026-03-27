# Visualize the results of a data validation

Visualize the results of a data validation

## Usage

``` r
plot_res(
  res,
  main = "Verification Results per Rule",
  colors = c(pass = "#308344", fail = "#E66820"),
  labels = TRUE,
  table = TRUE
)
```

## Arguments

- res:

  a data.frame as returned by
  [`check_data()`](https://davzim.github.io/dataverifyr/reference/check_data.md)

- main:

  the title of the plot

- colors:

  a named list of colors, with the names pass and fail

- labels:

  whether the values should be displayed on the barplot

- table:

  show a table in the legend with the values

## Value

a base r plot

## Examples

``` r
rs <- ruleset(
  rule(Ozone > 0 & Ozone < 120, allow_na = TRUE), # some mising values and > 120
  rule(Solar.R > 0, allow_na = TRUE),
  rule(Solar.R < 200, allow_na = TRUE),
  rule(Wind > 10),
  rule(Temp < 100)
)

res <- check_data(airquality, rs)
plot_res(res)
```
