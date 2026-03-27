# Filters a result dataset for the values that failed the verification

Filters a result dataset for the values that failed the verification

## Usage

``` r
filter_fails(res, x, per_rule = FALSE)
```

## Arguments

- res:

  a result data.frame as outputted from
  [`check_data()`](https://davzim.github.io/dataverifyr/reference/check_data.md)
  or a ruleset

- x:

  a dataset that was used in
  [`check_data()`](https://davzim.github.io/dataverifyr/reference/check_data.md)

- per_rule:

  if set to TRUE, a list of filtered data is returned, one for each
  failed verification rule. If set to FALSE, a data.frame is returned of
  the values that fail any rule.

## Value

the dataset with the entries that did not match the given rules

## Examples

``` r
rules <- ruleset(
  rule(mpg > 10 & mpg < 30), # mpg goes up to 34
  rule(cyl %in% c(4, 8)), # missing 6 cyl
  rule(vs %in% c(0, 1), allow_na = TRUE)
)

res <- check_data(mtcars, rules)

filter_fails(res, mtcars)
#>       mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>     <num> <num> <num> <num> <num> <num> <num> <num> <num> <num> <num>
#>  1:  32.4     4  78.7    66  4.08 2.200 19.47     1     1     4     1
#>  2:  30.4     4  75.7    52  4.93 1.615 18.52     1     1     4     2
#>  3:  33.9     4  71.1    65  4.22 1.835 19.90     1     1     4     1
#>  4:  30.4     4  95.1   113  3.77 1.513 16.90     1     1     5     2
#>  5:  21.0     6 160.0   110  3.90 2.620 16.46     0     1     4     4
#>  6:  21.0     6 160.0   110  3.90 2.875 17.02     0     1     4     4
#>  7:  21.4     6 258.0   110  3.08 3.215 19.44     1     0     3     1
#>  8:  18.1     6 225.0   105  2.76 3.460 20.22     1     0     3     1
#>  9:  19.2     6 167.6   123  3.92 3.440 18.30     1     0     4     4
#> 10:  17.8     6 167.6   123  3.92 3.440 18.90     1     0     4     4
#> 11:  19.7     6 145.0   175  3.62 2.770 15.50     0     1     5     6
filter_fails(res, mtcars, per_rule = TRUE)
#> $`mpg > 10 & mpg < 30`
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <num> <num> <num> <num> <num> <num> <num> <num> <num> <num> <num>
#> 1:  32.4     4  78.7    66  4.08 2.200 19.47     1     1     4     1
#> 2:  30.4     4  75.7    52  4.93 1.615 18.52     1     1     4     2
#> 3:  33.9     4  71.1    65  4.22 1.835 19.90     1     1     4     1
#> 4:  30.4     4  95.1   113  3.77 1.513 16.90     1     1     5     2
#> 
#> $`cyl %in% c(4, 8)`
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <num> <num> <num> <num> <num> <num> <num> <num> <num> <num> <num>
#> 1:  21.0     6 160.0   110  3.90 2.620 16.46     0     1     4     4
#> 2:  21.0     6 160.0   110  3.90 2.875 17.02     0     1     4     4
#> 3:  21.4     6 258.0   110  3.08 3.215 19.44     1     0     3     1
#> 4:  18.1     6 225.0   105  2.76 3.460 20.22     1     0     3     1
#> 5:  19.2     6 167.6   123  3.92 3.440 18.30     1     0     4     4
#> 6:  17.8     6 167.6   123  3.92 3.440 18.90     1     0     4     4
#> 7:  19.7     6 145.0   175  3.62 2.770 15.50     0     1     5     6
#> 

# alternatively, the first argument can also be a ruleset
filter_fails(rules, mtcars)
#>       mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>     <num> <num> <num> <num> <num> <num> <num> <num> <num> <num> <num>
#>  1:  32.4     4  78.7    66  4.08 2.200 19.47     1     1     4     1
#>  2:  30.4     4  75.7    52  4.93 1.615 18.52     1     1     4     2
#>  3:  33.9     4  71.1    65  4.22 1.835 19.90     1     1     4     1
#>  4:  30.4     4  95.1   113  3.77 1.513 16.90     1     1     5     2
#>  5:  21.0     6 160.0   110  3.90 2.620 16.46     0     1     4     4
#>  6:  21.0     6 160.0   110  3.90 2.875 17.02     0     1     4     4
#>  7:  21.4     6 258.0   110  3.08 3.215 19.44     1     0     3     1
#>  8:  18.1     6 225.0   105  2.76 3.460 20.22     1     0     3     1
#>  9:  19.2     6 167.6   123  3.92 3.440 18.30     1     0     4     4
#> 10:  17.8     6 167.6   123  3.92 3.440 18.90     1     0     4     4
#> 11:  19.7     6 145.0   175  3.62 2.770 15.50     0     1     5     6
filter_fails(rules, mtcars, per_rule = TRUE)
#> $`mpg > 10 & mpg < 30`
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <num> <num> <num> <num> <num> <num> <num> <num> <num> <num> <num>
#> 1:  32.4     4  78.7    66  4.08 2.200 19.47     1     1     4     1
#> 2:  30.4     4  75.7    52  4.93 1.615 18.52     1     1     4     2
#> 3:  33.9     4  71.1    65  4.22 1.835 19.90     1     1     4     1
#> 4:  30.4     4  95.1   113  3.77 1.513 16.90     1     1     5     2
#> 
#> $`cyl %in% c(4, 8)`
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <num> <num> <num> <num> <num> <num> <num> <num> <num> <num> <num>
#> 1:  21.0     6 160.0   110  3.90 2.620 16.46     0     1     4     4
#> 2:  21.0     6 160.0   110  3.90 2.875 17.02     0     1     4     4
#> 3:  21.4     6 258.0   110  3.08 3.215 19.44     1     0     3     1
#> 4:  18.1     6 225.0   105  2.76 3.460 20.22     1     0     3     1
#> 5:  19.2     6 167.6   123  3.92 3.440 18.30     1     0     4     4
#> 6:  17.8     6 167.6   123  3.92 3.440 18.90     1     0     4     4
#> 7:  19.7     6 145.0   175  3.62 2.770 15.50     0     1     5     6
#> 
```
