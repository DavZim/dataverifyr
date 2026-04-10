# Creates a set of rules

Creates a set of rules

## Usage

``` r
ruleset(..., data_columns = NULL, meta = NULL, data_name = NULL)

# S3 method for class 'ruleset'
print(x, n = 3, ...)
```

## Arguments

- ...:

  a list of rules

- data_columns:

  optional list of schema declarations created with internal
  [`data_column()`](https://davzim.github.io/dataverifyr/reference/data_column.md)
  helper.

- meta:

  optional metadata list for v1 YAML workflows.

- data_name:

  optional name of the primary dataset when
  [`check_data()`](https://davzim.github.io/dataverifyr/reference/check_data.md)
  receives a named list of datasets.

- x:

  a ruleset to print

- n:

  a maximum number of rules to print

## Value

the list of rules as a ruleset

## Methods (by generic)

- `print(ruleset)`: Prints a ruleset

## Examples

``` r
r1 <- rule(mpg > 10)
r2 <- rule(mpg < 20)
rs <- ruleset(r1, r2)
rs
#> <Verification Ruleset with 2 elements>
#>   [1] 'Rule for: mpg' matching `mpg > 10` (allow_na: FALSE)
#>   [2] 'Rule for: mpg' matching `mpg < 20` (allow_na: FALSE)

rs <- ruleset(
  rule(cyl %in% c(4, 6, 8)),
  rule(is.numeric(disp))
)
rs
#> <Verification Ruleset with 2 elements>
#>   [1] 'Rule for: cyl' matching `cyl %in% c(4, 6, 8)` (allow_na: FALSE)
#>   [2] 'Rule for: disp' matching `is.numeric(disp)` (allow_na: FALSE)

# combine row, schema, and relational checks
orders <- data.frame(order_id = 1:4, customer_id = c(10, 11, 99, NA), amount = c(10, 20, -5, 30))
customers <- data.frame(customer_id = c(10, 11, 12))

rs2 <- ruleset(
  rule(amount >= 0, name = "amount must be non-negative"),
  reference_rule(
    local_col = "customer_id",
    ref_dataset = "customers",
    ref_col = "customer_id",
    allow_na = TRUE
  ),
  data_columns = list(
    data_column("order_id", type = "int", optional = FALSE),
    data_column("customer_id", type = "int", optional = FALSE),
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
#> 4:         schema          Schema: column 'customer_id' has type 'int'
#> 5:         schema                       Schema: column 'amount' exists
#> 6:         schema            Schema: column 'amount' has type 'double'
#> 7:       row_rule                          amount must be non-negative
#> 8: reference_rule Reference rule: customer_id in customers$customer_id
#>                                      expr allow_na negate tests  pass  fail
#>                                    <char>   <lgcl> <lgcl> <int> <int> <int>
#> 1:              column_exists('order_id')    FALSE  FALSE     1     1     0
#> 2:       column_type('order_id') == 'int'    FALSE  FALSE     1     1     0
#> 3:           column_exists('customer_id')    FALSE  FALSE     1     1     0
#> 4:    column_type('customer_id') == 'int'    FALSE  FALSE     1     0     1
#> 5:                column_exists('amount')    FALSE  FALSE     1     1     0
#> 6:      column_type('amount') == 'double'    FALSE  FALSE     1     1     0
#> 7:                            amount >= 0    FALSE  FALSE     4     3     1
#> 8: customer_id %in% customers$customer_id     TRUE  FALSE     4     3     1
#>      warn                                                    error
#>    <char>                                                   <char>
#> 1:                                                                
#> 2:                                                                
#> 3:                                                                
#> 4:        Column 'customer_id' does not match declared type 'int'.
#> 5:                                                                
#> 6:                                                                
#> 7:                                                                
#> 8:                                                                
#>                 time
#>           <difftime>
#> 1: 0.000000e+00 secs
#> 2: 0.000000e+00 secs
#> 3: 0.000000e+00 secs
#> 4: 0.000000e+00 secs
#> 5: 0.000000e+00 secs
#> 6: 0.000000e+00 secs
#> 7: 4.155636e-04 secs
#> 8: 5.841255e-05 secs
```
