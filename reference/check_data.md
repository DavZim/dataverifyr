# Checks if a dataset confirms to a given set of rules

Checks if a dataset confirms to a given set of rules

## Usage

``` r
check_data(
  x,
  rules,
  xname = deparse(substitute(x)),
  stop_on_fail = FALSE,
  stop_on_warn = FALSE,
  stop_on_error = FALSE
)
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

- rules:

  a list of
  [`rule`](https://davzim.github.io/dataverifyr/reference/rule.md)s

- xname:

  optional, a name for the x variable (only used for errors)

- stop_on_fail:

  when any of the rules fail, throw an error with stop

- stop_on_warn:

  when a warning is found in the code execution, throw an error with
  stop

- stop_on_error:

  when an error is found in the code execution, throw an error with stop

## Value

a data.frame-like object with one row for each rule and its results

## See also

[`detect_backend()`](https://davzim.github.io/dataverifyr/reference/detect_backend.md)

## Examples

``` r
rs <- ruleset(
  rule(mpg > 10),
  rule(cyl %in% c(4, 6)), # missing 8
  rule(qsec >= 14.5 & qsec <= 22.9)
)
rs
#> <Verification Ruleset with 3 elements>
#>   [1] 'Rule for: mpg' matching `mpg > 10` (allow_na: FALSE)
#>   [2] 'Rule for: cyl' matching `cyl %in% c(4, 6)` (allow_na: FALSE)
#>   [3] 'Rule for: qsec' matching `qsec >= 14.5 & qsec <= 22.9` (allow_na: FALSE)

check_data(mtcars, rs)
#>              name                        expr allow_na negate tests  pass  fail
#>            <char>                      <char>   <lgcl> <lgcl> <int> <int> <int>
#> 1:  Rule for: mpg                    mpg > 10    FALSE  FALSE    32    32     0
#> 2:  Rule for: cyl            cyl %in% c(4, 6)    FALSE  FALSE    32    18    14
#> 3: Rule for: qsec qsec >= 14.5 & qsec <= 22.9    FALSE  FALSE    32    32     0
#>      warn  error              time
#>    <char> <char>        <difftime>
#> 1:               0.0048604012 secs
#> 2:               0.0024776459 secs
#> 3:               0.0003242493 secs
```
