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
  stop_on_error = FALSE,
  stop_on_schema_fail = FALSE,
  extra_columns = c("ignore", "warn", "fail")
)
```

## Arguments

- x:

  a dataset, either a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html),
  [`dplyr::tibble`](https://dplyr.tidyverse.org/reference/reexports.html),
  [`data.table::data.table`](https://rdrr.io/pkg/data.table/man/data.table.html),
  [`arrow::arrow_table`](https://arrow.apache.org/docs/r/reference/table.html),
  [`arrow::open_dataset`](https://arrow.apache.org/docs/r/reference/open_dataset.html),
  or [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html) (SQL
  connection). Can also be a named list of datasets when using reference
  rules.

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

- stop_on_schema_fail:

  when any schema checks fail, throw an error with stop

- extra_columns:

  how to treat columns in `x` that are not declared in optional
  `data_columns` attached to a ruleset. One of `"ignore"` (default),
  `"warn"`, or `"fail"`.

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
#>    check_type           name                        expr allow_na negate tests
#>        <char>         <char>                      <char>   <lgcl> <lgcl> <int>
#> 1:   row_rule  Rule for: mpg                    mpg > 10    FALSE  FALSE    32
#> 2:   row_rule  Rule for: cyl            cyl %in% c(4, 6)    FALSE  FALSE    32
#> 3:   row_rule Rule for: qsec qsec >= 14.5 & qsec <= 22.9    FALSE  FALSE    32
#>     pass  fail   warn  error              time
#>    <int> <int> <char> <char>        <difftime>
#> 1:    32     0               0.0047128201 secs
#> 2:    18    14               0.0024578571 secs
#> 3:    32     0               0.0003011227 secs

# schema + relation checks in one output
orders <- data.frame(order_id = 1:3, customer_id = c(10, 99, NA), amount = c(10, -5, 20))
customers <- data.frame(customer_id = c(10, 11))

rs2 <- ruleset(
  rule(amount >= 0, name = "amount non-negative"),
  reference_rule(
    local_col = "customer_id",
    ref_dataset = "customers",
    ref_col = "customer_id",
    allow_na = TRUE
  ),
  data_columns = list(
    data_column("order_id", type = "int", optional = FALSE),
    data_column("customer_id", type = "double", optional = FALSE),
    data_column("amount", type = "double", optional = FALSE)
  ),
  data_name = "orders"
)

check_data(list(orders = orders, customers = customers), rs2)
#>        check_type                                                 name
#>            <char>                                               <char>
#> 1:         schema                     Schema: column 'order_id' exists
#> 2:         schema             Schema: column 'order_id' has type 'int'
#> 3:         schema                  Schema: column 'customer_id' exists
#> 4:         schema       Schema: column 'customer_id' has type 'double'
#> 5:         schema                       Schema: column 'amount' exists
#> 6:         schema            Schema: column 'amount' has type 'double'
#> 7:       row_rule                                  amount non-negative
#> 8: reference_rule Reference rule: customer_id in customers$customer_id
#>                                      expr allow_na negate tests  pass  fail
#>                                    <char>   <lgcl> <lgcl> <int> <int> <int>
#> 1:              column_exists('order_id')    FALSE  FALSE     1     1     0
#> 2:       column_type('order_id') == 'int'    FALSE  FALSE     1     1     0
#> 3:           column_exists('customer_id')    FALSE  FALSE     1     1     0
#> 4: column_type('customer_id') == 'double'    FALSE  FALSE     1     1     0
#> 5:                column_exists('amount')    FALSE  FALSE     1     1     0
#> 6:      column_type('amount') == 'double'    FALSE  FALSE     1     1     0
#> 7:                            amount >= 0    FALSE  FALSE     3     2     1
#> 8: customer_id %in% customers$customer_id     TRUE  FALSE     3     2     1
#>      warn  error              time
#>    <char> <char>        <difftime>
#> 1:               0.0000000000 secs
#> 2:               0.0000000000 secs
#> 3:               0.0000000000 secs
#> 4:               0.0000000000 secs
#> 5:               0.0000000000 secs
#> 6:               0.0000000000 secs
#> 7:               0.0002963543 secs
#> 8:               0.0000500679 secs
```
