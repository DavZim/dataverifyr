# Read and write rules to a yaml file

Read and write rules to a yaml file

## Usage

``` r
write_rules(x, file, format = c("v1", "pre_v1"))

read_rules(file)
```

## Arguments

- x:

  a list of rules

- file:

  a filename

- format:

  output format. `"v1"` writes structured YAML with `meta`,
  `data-columns`, and `data-rules`. `"pre_v1"` keeps the pre package
  version 1.0 flat-list structure.

## Value

the filename invisibly

## Functions

- `read_rules()`: reads a ruleset back in

## Examples

``` r
rr <- ruleset(
  rule(mpg > 10),
  rule(cyl %in% c(4, 6, 8))
)
file <- tempfile(fileext = ".yml")
write_rules(rr, file)
```
