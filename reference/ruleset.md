# Creates a set of rules

Creates a set of rules

## Usage

``` r
ruleset(...)

# S3 method for class 'ruleset'
print(x, n = 3, ...)
```

## Arguments

- ...:

  a list of rules

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
```
