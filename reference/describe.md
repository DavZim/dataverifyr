# Describes a dataset

Note that the current version is in the beta stadium at best, that means
the R-native formats (data.frame, dplyr/tibble, or data.table) are a lot
faster than arrow or SQL-based datasets.

## Usage

``` r
describe(x)
```

## Arguments

- x:

  a dataset, either a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),
  [`dplyr::tibble`](https://dplyr.tidyverse.org/reference/reexports.html),
  [`data.table::data.table`](https://rdrr.io/pkg/data.table/man/data.table.html),
  [`arrow::arrow_table`](https://arrow.apache.org/docs/r/reference/Table-class.html),
  [`arrow::open_dataset`](https://arrow.apache.org/docs/r/reference/open_dataset.html),
  or [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html) (SQL
  connection)

## Value

a `data.frame`,
[`dplyr::tibble`](https://tibble.tidyverse.org/reference/tibble.html),
or
[`data.table::data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
containing a summary of the dataset given

## See also

Similar to
[skimr::skim()](https://cran.r-project.org/web/packages/skimr/vignettes/skimr.html),
[summarytools::dfSummary()](https://cran.r-project.org/web/packages/summarytools/vignettes/introduction.html#data-frame-summaries-dfsummary),
and
[gtExtras::gt_plt_summary()](https://jthomasmock.github.io/gtExtras/reference/gt_plt_summary.html)

## Examples

``` r
describe(mtcars)
#>        var    type     n n_distinct  n_na                  most_frequent    min
#>     <char>  <char> <int>      <int> <int>                         <char>  <num>
#>  1:    mpg numeric    32         25     0     21 (2), 22.8 (2), 21.4 (2) 10.400
#>  2:    cyl numeric    32          3     0          8 (14), 4 (11), 6 (7)  4.000
#>  3:   disp numeric    32         27     0    275.8 (3), 160 (2), 360 (2) 71.100
#>  4:     hp numeric    32         22     0      110 (3), 175 (3), 180 (3) 52.000
#>  5:   drat numeric    32         22     0    3.92 (3), 3.07 (3), 3.9 (2)  2.760
#>  6:     wt numeric    32         29     0   3.44 (3), 3.57 (2), 2.62 (1)  1.513
#>  7:   qsec numeric    32         30     0 17.02 (2), 18.9 (2), 16.46 (1) 14.500
#>  8:     vs numeric    32          2     0        0 (18), 1 (14), NA (NA)  0.000
#>  9:     am numeric    32          2     0        0 (19), 1 (13), NA (NA)  0.000
#> 10:   gear numeric    32          3     0          3 (15), 4 (12), 5 (5)  3.000
#> 11:   carb numeric    32          6     0          4 (10), 2 (10), 1 (7)  1.000
#>           mean  median     max          sd
#>          <num>   <num>   <num>       <num>
#>  1:  20.090625  19.200  33.900   6.0269481
#>  2:   6.187500   6.000   8.000   1.7859216
#>  3: 230.721875 196.300 472.000 123.9386938
#>  4: 146.687500 123.000 335.000  68.5628685
#>  5:   3.596563   3.695   4.930   0.5346787
#>  6:   3.217250   3.325   5.424   0.9784574
#>  7:  17.848750  17.710  22.900   1.7869432
#>  8:   0.437500   0.000   1.000   0.5040161
#>  9:   0.406250   0.000   1.000   0.4989909
#> 10:   3.687500   4.000   5.000   0.7378041
#> 11:   2.812500   2.000   8.000   1.6152000
```
