# Sample Orders Dataset for Examples and Tests

A small, human-readable dataset with mixed column types, missing values,
and one datetime column. It is designed for documentation examples and
unit tests.

## Usage

``` r
sample_data
```

## Format

A data frame with 8 rows and 6 variables:

- order_id:

  Integer order identifier.

- customer_tier:

  Character tier (`"bronze"`, `"silver"`, `"gold"`, etc), includes one
  `NA`.

- amount:

  Numeric order amount, includes one negative value and one `NA`.

- paid:

  Logical payment flag, includes one `NA`.

- payment_method:

  Character payment method, includes one `NA`.

- order_time:

  `POSIXct` order timestamp in UTC, includes one `NA`.

## Examples

``` r
sample_data
#>   order_id customer_tier amount  paid payment_method          order_time
#> 1        1          gold 120.50  TRUE           card 2025-01-01 09:00:00
#> 2        2        silver  80.00  TRUE           cash 2025-01-02 10:30:00
#> 3        3        bronze  -5.00 FALSE           none 2025-01-03 12:15:00
#> 4        4          gold 320.25  TRUE           card 2025-01-04 15:45:00
#> 5        5          <NA>  45.10 FALSE           none                <NA>
#> 6        6        silver     NA    NA           card 2025-01-06 08:10:00
#> 7        7        bronze   0.00  TRUE           <NA> 2025-01-07 17:20:00
#> 8        8       unknown  99.99  TRUE           none 2025-01-08 11:05:00
```
