# Define a Relational Reference Rule

Creates a rule that checks whether values in a local column exist in a
column of a referenced dataset. Use with
[`check_data()`](https://davzim.github.io/dataverifyr/reference/check_data.md)
by supplying `x` as a named list of datasets and setting `data_name` in
[`ruleset()`](https://davzim.github.io/dataverifyr/reference/ruleset.md)
(or by ordering the list so the first entry is the primary dataset).

## Usage

``` r
reference_rule(
  local_col,
  ref_dataset,
  ref_col,
  name = NA,
  allow_na = FALSE,
  negate = FALSE,
  ...
)
```

## Arguments

- local_col:

  column name in the primary dataset.

- ref_dataset:

  name of the referenced dataset in the `x` list.

- ref_col:

  column name in the referenced dataset.

- name:

  optional display name for the rule.

- allow_na:

  logical; if `TRUE`, missing values in `local_col` are treated as
  passing.

- negate:

  logical; if `TRUE`, inverts the rule (values must *not* be in the
  referenced column).

- ...:

  additional fields attached to the rule object.

## Value

A `reference_rule` object that can be included in
[`ruleset()`](https://davzim.github.io/dataverifyr/reference/ruleset.md).

## Examples

``` r
flights <- data.frame(carrier = c("AA", "BB", NA_character_))
carriers <- data.frame(carrier_id = c("AA"))

rs <- ruleset(
  reference_rule(
    local_col = "carrier",
    ref_dataset = "carriers",
    ref_col = "carrier_id",
    allow_na = TRUE
  ),
  data_name = "flights"
)

check_data(list(flights = flights, carriers = carriers), rs)
#>        check_type                                           name
#>            <char>                                         <char>
#> 1: reference_rule Reference rule: carrier in carriers$carrier_id
#>                                expr allow_na negate tests  pass  fail   warn
#>                              <char>   <lgcl> <lgcl> <int> <int> <int> <char>
#> 1: carrier %in% carriers$carrier_id     TRUE  FALSE     3     2     1       
#>     error              time
#>    <char>        <difftime>
#> 1:        5.578995e-05 secs

# negated relation: value must NOT exist in blacklist
blacklist <- data.frame(carrier_id = c("XX", "YY"))
rs_neg <- ruleset(
  reference_rule(
    local_col = "carrier",
    ref_dataset = "blacklist",
    ref_col = "carrier_id",
    negate = TRUE,
    allow_na = TRUE
  ),
  data_name = "flights"
)

check_data(list(flights = flights, blacklist = blacklist), rs_neg)
#>        check_type                                            name
#>            <char>                                          <char>
#> 1: reference_rule Reference rule: carrier in blacklist$carrier_id
#>                                 expr allow_na negate tests  pass  fail   warn
#>                               <char>   <lgcl> <lgcl> <int> <int> <int> <char>
#> 1: carrier %in% blacklist$carrier_id     TRUE   TRUE     3     2     1       
#>     error              time
#>    <char>        <difftime>
#> 1:        5.459785e-05 secs
```
