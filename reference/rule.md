# Creates a single data rule

Creates a single data rule

## Usage

``` r
rule(expr, name = NA, allow_na = FALSE, negate = FALSE, ...)

# S3 method for class 'rule'
print(x, ...)
```

## Arguments

- expr:

  an expression which dictates which determines when a rule is good.
  Note that the expression is evaluated in
  [`check_data()`](https://davzim.github.io/dataverifyr/reference/check_data.md),
  within the given framework. That means, for example if a the data
  given to
  [`check_data()`](https://davzim.github.io/dataverifyr/reference/check_data.md)
  is an `arrow` dataset, the expression must be mappable from `arrow`
  (see also [arrow
  documentation](https://arrow.apache.org/docs/r/reference/acero.html#function-mappings)).
  The expression can be given as a string as well.

- name:

  an optional name for the rule for reference

- allow_na:

  does the rule allow for NA values in the data? default value is FALSE.
  Note that when NAs are introduced in the expression, `allow_na` has no
  effect. Eg when the rule `as.numeric(vs) %in% c(0, 1)` finds the
  values of `vs` as `c("1", "A")`, the rule will throw a fail regardless
  of the value of `allow_na` as the NA is introduced in the expression
  and is not found in the original data. However, when the values of
  `vs` are `c("1", NA)`, `allow_na` will have an effect.

- negate:

  is the rule negated, only applies to the expression not allow_na, that
  is, if `expr = mpg > 10`, `allow_na = TRUE`, and `negate = TRUE`, it
  would match all `mpg <= 10` as well as NAs.

- ...:

  additional arguments that are carried along for your documentation,
  but are not used. Could be for example date, person, contact, comment,
  etc

- x:

  a rule to print

## Value

The rule values as a list

## Methods (by generic)

- `print(rule)`: Prints a rule

## Examples

``` r
r <- rule(mpg > 10)
r
#> <Verification Rule>
#>   expr: 'mpg > 10'
#>   name: 'Rule for: mpg'
#>   allow NA: FALSE
#>   negated:  FALSE

r2 <- rule(mpg > 10, name = "check that mpg is reasonable", allow_na = TRUE,
           negate = FALSE, author = "me", date = Sys.Date())
r2
#> <Verification Rule>
#>   expr: 'mpg > 10'
#>   name: 'check that mpg is reasonable'
#>   allow NA: TRUE
#>   negated:  FALSE
#>   author: 'me'
#>   date: '2026-03-27'

check_data(mtcars, r)
#>             name     expr allow_na negate tests  pass  fail   warn  error
#>           <char>   <char>   <lgcl> <lgcl> <int> <int> <int> <char> <char>
#> 1: Rule for: mpg mpg > 10    FALSE  FALSE    32    32     0              
#>                 time
#>           <difftime>
#> 1: 0.0004281998 secs

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
#> 1:               0.0003235340 secs
#> 2:               0.0010995865 secs
#> 3:               0.0002834797 secs
```
