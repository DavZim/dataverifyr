
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dataverifyr - a lightweight, flexible, and fast data validation package that can handle all datasizes

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/dataverifyr)](https://www.r-pkg.org/pkg/dataverifyr)
[![R-CMD-check](https://github.com/DavZim/dataverifyr/workflows/R-CMD-check/badge.svg)](https://github.com/DavZim/dataverifyr/actions)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/dataverifyr)](https://www.r-pkg.org/pkg/dataverifyr)

<!-- badges: end -->

The goal of `dataverifyr` is to allow a wide variety of flexible data
validation checks. That means, you can specify the rules/expressions in
a yaml file, load it into R and compare any arbitrary dataset against
it. The package is built in such a way, that you can use `base-R`,
`dplyr`, `data.table`, `arrow` (both as an `arrow_table` as well as an
`open_dataset()` connection), but also any `DBI`-compliant database
(currently tested with `RSQLite`).

The package is lightweight as all the heavy dependencies are
Suggests-only, that means if you want to use `data.table` for the task,
you don’t need to install the other packages (`arrow`, `DBI`, etc)
unless you explicitly tell R to do so when installing the package.

The backend for your analysis is automatically chosen based on the input
dataset as well as the available packages. By using the underlying
technologies and handing all evaluation of code to the backends, this
package can deal with all datasizes the backends can deal with.

## Installation

You can install the development version of `dataverifyr` like so:

``` r
# development version
# devtools::install_github("DavZim/dataverifyr")

# CRAN release
install.packages("dataverifyr")
```

## Example

This is a basic example which shows you how to

1.  create a rule-set manually
2.  check if a dataset matches all given rules
3.  save the rules to a yaml-file for better maintainability
4.  load the rules from disk again

``` r
library(dataverifyr)

rules <- ruleset(
  rule(mpg > 10 & mpg < 40),
  rule(cyl %in% c(4, 6, 8)),
  rule(vs %in% c(0, 1), allow_na = TRUE)
)

# print the rules
rules
#> <Verification Ruleset with 3 elements>
#>   [1] 'Rule for: ' matching `mpg > 10 & mpg < 40` (allow_na: FALSE)
#>   [2] 'Rule for: ' matching `cyl %in% c(4, 6, 8)` (allow_na: FALSE)
#>   [3] 'Rule for: ' matching `vs %in% c(0, 1)` (allow_na: TRUE)

# check if the data matches our rules
res <- check_data(mtcars, rules)
res
#>          name                expr tests pass fail warn
#> 1: Rule for:  mpg > 10 & mpg < 40    32   32    0     
#> 2: Rule for:  cyl %in% c(4, 6, 8)    32   32    0     
#> 3: Rule for:      vs %in% c(0, 1)    32    0   32     
#>                                             error             time
#> 1:                                                0.004320860 secs
#> 2:                                                0.003097057 secs
#> 3: 0 arguments passed to 'is.na' which requires 1 0.007634878 secs

# save the rules to yml
write_rules(rules, "example_rules.yaml")

cat(paste(readLines("example_rules.yaml"), collapse = "\n"))
#> - name: 'Rule for: '
#>   expr: mpg > 10 & mpg < 40
#>   allow_na: no
#>   negate: no
#>   index: 1
#> - name: 'Rule for: '
#>   expr: cyl %in% c(4, 6, 8)
#>   allow_na: no
#>   negate: no
#>   index: 2
#> - name: 'Rule for: '
#>   expr: vs %in% c(0, 1)
#>   allow_na: yes
#>   negate: no
#>   index: 3

r2 <- read_rules("example_rules.yaml")
identical(rules, r2)
#> [1] FALSE
```

# Backends

At the moment the following backends are supported. Note that they are
automatically chosen based on filetype (eg, when giving a `dplyr::tbl()`
pointing to an sqlite database, the package will automatically choose
`DBI`/`dplyr` for the task) and package availability.

| Backend                | Library                                                                 | Status  | Comment                                                                                                                                            |
|------------------------|-------------------------------------------------------------------------|---------|----------------------------------------------------------------------------------------------------------------------------------------------------|
| `base-R`               | `base`                                                                  | ✔️      | When `data.table` or `dplyr` are installed, they are leveraged for higher performance, but if requried, the package works with base-r only         |
| `dplyr`                | [`dplyr`](https://dplyr.tidyverse.org/)                                 | ✔️      |                                                                                                                                                    |
| `data.table`           | [`data.table`](r-datatable.com)                                         | ✔️      | Used as the default when installed and no special data-format is given.                                                                            |
| `Table`/`ArrowTabular` | [`arrow`](https://arrow.apache.org/docs/r/)                             | ✔️      | For example by using `arrow::arrow_table()`                                                                                                        |
| `FileSystemDataset`    | [`arrow`](https://arrow.apache.org/docs/r/)                             | ✔️      | For on disk storage using `arrow::open_dataset()`, very good for large datasets. See example below.                                                |
| `sqlite`               | [`RSQLite`](https://rsqlite.r-dbi.org/)/[`DBI`](https://dbi.r-dbi.org/) | ✔️      | Note that missing values are converted to `0`s when using sqlite by default ([c.f. this SO answer](https://stackoverflow.com/a/57746647/3048453)). |
| `DuckDB`               | [`duckdb`](https://duckdb.org/docs/api/r.html)                          | ❌️ / ❓ | Not tested, but should work out-of-the-box through [`DBI`](https://dbi.r-dbi.org/)                                                                 |
| `postgreSQL`           | [`RPostgres`](https://rpostgres.r-dbi.org/)                             | ❌️ / ❓ | Not tested, but should work out-of-the-box through [`DBI`](https://dbi.r-dbi.org/)                                                                 |

## larger `arrow` example

For a more involved example, using a different backend, let’s say we
have a larger dataset of taxi trips from NY (see also the official
[datasource](https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page)),
that we have saved as a local arrow dataset (using parquet as a data
format).

### 1 Download and Prepare Data

First we prepare the data:

``` r
library(arrow)
url <- "https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_2018-01.parquet"
file <- "yellow-taxi-2018-jan.parquet"
if (!file.exists(file)) download.file(url, file, method = "curl")
file.size(file) / 1e6 # in MB
#> [1] 123.6685

# quick check of the filesize
d <- read_parquet(file)
dim(d)
#> [1] 8760687      19
names(d)
#>  [1] "VendorID"              "tpep_pickup_datetime"  "tpep_dropoff_datetime"
#>  [4] "passenger_count"       "trip_distance"         "RatecodeID"           
#>  [7] "store_and_fwd_flag"    "PULocationID"          "DOLocationID"         
#> [10] "payment_type"          "fare_amount"           "extra"                
#> [13] "mta_tax"               "tip_amount"            "tolls_amount"         
#> [16] "improvement_surcharge" "total_amount"          "congestion_surcharge" 
#> [19] "airport_fee"

# write the dataset to disk
write_dataset(d, "nyc-taxi-data")
```

### 2 Create Rules

Then we can create some rules that we want to see if our data conforms
to. Ideally this would be done in the yaml-file directly to separate the
execution code from the data, but for now we will do it here in R
(replace the `rules` definition with
`rules <- read_rules("myrules.yaml")` to load the rules from disk).

``` r
rules <- ruleset(
  rule(passenger_count >= 0 & passenger_count <= 10),
  rule(trip_distance >= 0 & trip_distance <= 1000), # limit to reasonable distances
  rule(payment_type %in% c(0, 1, 2, 3, 4))
)
rules
#> <Verification Ruleset with 3 elements>
#>   [1] 'Rule for: ' matching `passenger_count >= 0 & passenger_count <= 10` (allow_na: FALSE)
#>   [2] 'Rule for: ' matching `trip_distance >= 0 & trip_distance <= 1000` (allow_na: FALSE)
#>   [3] 'Rule for: ' matching `payment_type %in% c(0, 1, 2, 3, 4)` (allow_na: FALSE)
```

### 3 Check that the Data is Valid

Now we can check if the data follows our rules or if we have unexpected
data points:

``` r
# open the dataset 
ds <- open_dataset("nyc-taxi-data/")

# perform the data validation check
res <- check_data(ds, rules)
res
#> # A tibble: 3 × 8
#>   name         expr                        tests    pass  fail warn  error time 
#>   <chr>        <chr>                       <int>   <int> <int> <chr> <chr> <drt>
#> 1 "Rule for: " passenger_count >= 0 & p… 8760687 8760687     0 ""    ""    0.73…
#> 2 "Rule for: " trip_distance >= 0 & tri… 8760687 8760686     1 ""    ""    0.48…
#> 3 "Rule for: " payment_type %in% c(0, 1… 8760687 8760687     0 ""    ""    0.44…
```

Using the power of `arrow`, we were able to scan 8+mln observations for
three rules in about 1.5 seconds (YMMV). As we can see from the results,
there is one unexpected value, lets quickly investigate

``` r
ds |> 
  dplyr::filter(trip_distance < 0 | trip_distance > 1000) |> 
  dplyr::collect() |> 
  dplyr::select(tpep_pickup_datetime, tpep_dropoff_datetime, trip_distance)
#> # A tibble: 1 × 3
#>   tpep_pickup_datetime tpep_dropoff_datetime trip_distance
#>   <dttm>               <dttm>                        <dbl>
#> 1 2018-01-30 12:41:02  2018-01-30 12:42:09         189484.
```

As we can see, there is probably a data error (a trip distance of 190k
miles in 1 minute seems - ehm stellar…).

# Alternative Data Validation R Libraries

If this library is not what you are looking for, the following might be
good alternatives:

- [`pointblank`](https://rich-iannone.github.io/pointblank/)
- [`validate`](https://github.com/data-cleaning/validate)
- [`data.validator`](https://github.com/Appsilon/data.validator)
