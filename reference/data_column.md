# Define a Column Specification for Schema Checks

Creates a single column declaration used in
`ruleset(..., data_columns = ...)`. Column declarations are schema
checks (column existence, optionality, and declared type), whereas
[`rule()`](https://davzim.github.io/dataverifyr/reference/rule.md) is
for row-wise value checks.

## Usage

``` r
data_column(
  col,
  type = NA_character_,
  optional = FALSE,
  description = NA_character_
)
```

## Arguments

- col:

  column name.

- type:

  optional declared type (for example `"int"`, `"double"`, `"str"`,
  `"logical"`). Use `NA_character_` for no type declaration.

- optional:

  logical; if `FALSE`, the column is required.

- description:

  optional free-text description.

## Value

A `data_column` object (list) that can be passed in
`ruleset(..., data_columns = list(...))`.

## Examples

``` r
rs <- ruleset(
  rule(price >= 0),
  data_columns = list(
    data_column("price", type = "double", optional = FALSE),
    data_column("note", type = "str", optional = TRUE)
  )
)
rs
#> <Verification Ruleset with 1 elements>
#>   [1] 'Rule for: price' matching `price >= 0` (allow_na: FALSE)

# combined with row rules and strict schema stopping
order_rules <- ruleset(
  rule(price >= 0, allow_na = FALSE),
  data_columns = list(
    data_column("order_id", type = "int", optional = FALSE),
    data_column("price", type = "double", optional = FALSE),
    data_column("note", type = "str", optional = TRUE)
  )
)

check_data(
  data.frame(order_id = 1:3, price = c(10, 20, 30), note = c("ok", NA, "ok")),
  order_rules,
  stop_on_schema_fail = TRUE
)
#>    check_type                                     name
#>        <char>                                   <char>
#> 1:     schema         Schema: column 'order_id' exists
#> 2:     schema Schema: column 'order_id' has type 'int'
#> 3:     schema            Schema: column 'price' exists
#> 4:     schema Schema: column 'price' has type 'double'
#> 5:     schema             Schema: column 'note' exists
#> 6:     schema     Schema: column 'note' has type 'str'
#> 7:   row_rule                          Rule for: price
#>                                expr allow_na negate tests  pass  fail   warn
#>                              <char>   <lgcl> <lgcl> <int> <int> <int> <char>
#> 1:        column_exists('order_id')    FALSE  FALSE     1     1     0       
#> 2: column_type('order_id') == 'int'    FALSE  FALSE     1     1     0       
#> 3:           column_exists('price')    FALSE  FALSE     1     1     0       
#> 4: column_type('price') == 'double'    FALSE  FALSE     1     1     0       
#> 5:            column_exists('note')    FALSE  FALSE     1     1     0       
#> 6:     column_type('note') == 'str'    FALSE  FALSE     1     1     0       
#> 7:                       price >= 0    FALSE  FALSE     3     3     0       
#>     error              time
#>    <char>        <difftime>
#> 1:        0.0000000000 secs
#> 2:        0.0000000000 secs
#> 3:        0.0000000000 secs
#> 4:        0.0000000000 secs
#> 5:        0.0000000000 secs
#> 6:        0.0000000000 secs
#> 7:        0.0004127026 secs
```
