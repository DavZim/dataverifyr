# Changelog

## dataverifyr 0.1.10

- Add
  [`describe()`](https://davzim.github.io/dataverifyr/reference/describe.md)
  to describe a dataset
- [`check_data()`](https://davzim.github.io/dataverifyr/reference/check_data.md)
  now includes schema checks in the output by default (`check_type` as
  first result column), including explicit rows for column existence and
  declared type checks
- add `stop_on_schema_fail` to
  [`check_data()`](https://davzim.github.io/dataverifyr/reference/check_data.md)
  to optionally stop when schema checks fail
- update
  [`filter_fails()`](https://davzim.github.io/dataverifyr/reference/filter_fails.md)
  to ignore schema/reference rows and only process row rules from
  [`check_data()`](https://davzim.github.io/dataverifyr/reference/check_data.md)
  results
- Add explicit regression test for
  [`detect_backend()`](https://davzim.github.io/dataverifyr/reference/detect_backend.md)
  fallback to `dplyr` when input is a `data.frame` and `data.table` is
  unavailable
- Add structured ruleset internals for schema metadata
  ([`data_column()`](https://davzim.github.io/dataverifyr/reference/data_column.md),
  `rule_meta()`) and reference checks
  ([`reference_rule()`](https://davzim.github.io/dataverifyr/reference/reference_rule.md))
- Extend
  [`ruleset()`](https://davzim.github.io/dataverifyr/reference/ruleset.md),
  [`check_data()`](https://davzim.github.io/dataverifyr/reference/check_data.md),
  [`read_rules()`](https://davzim.github.io/dataverifyr/reference/write_rules.md),
  and
  [`write_rules()`](https://davzim.github.io/dataverifyr/reference/write_rules.md)
  for v1 schema-aware workflows; keep
  [`rule()`](https://davzim.github.io/dataverifyr/reference/rule.md) as
  row-level API (no `col_rule()`)
- Add exported `sample_data` dataset (mixed types, NAs, datetime) for
  examples and tests
- export
  [`reference_rule()`](https://davzim.github.io/dataverifyr/reference/reference_rule.md)
  and extend examples in
  [`ruleset()`](https://davzim.github.io/dataverifyr/reference/ruleset.md),
  [`check_data()`](https://davzim.github.io/dataverifyr/reference/check_data.md),
  [`reference_rule()`](https://davzim.github.io/dataverifyr/reference/reference_rule.md),
  and
  [`data_column()`](https://davzim.github.io/dataverifyr/reference/data_column.md)
  to show combined schema + relational workflows
- Require DuckDB version `>= 1.5.1.9002` in all DuckDB-backed tests via
  `skip_if_not_installed("duckdb", "1.5.1.9002")`

## dataverifyr 0.1.9

- fix tests for new duckdb version (fixes
  [\#17](https://github.com/DavZim/dataverifyr/issues/17), thanks
  [@krlmlr](https://github.com/krlmlr) for reporting)

## dataverifyr 0.1.8

CRAN release: 2024-01-10

- fix broken URL Link to pointblank

## dataverifyr 0.1.7

- rename args `fail_on_X` to `stop_on_X`
- adds `stop_on_fail` to
  \[[`check_data()`](https://davzim.github.io/dataverifyr/reference/check_data.md)\],
  so that the examples using `read_custom()` make sense (eg in the
  Readme); thanks
  [FedericoComoglio](https://github.com/FedericoComoglio) for pointing
  it out!
- fix bug when dplyr backend is used + add tests; thanks
  [FedericoComoglio](https://github.com/FedericoComoglio) for reporting
  the bug!
- expose
  \[[`detect_backend()`](https://davzim.github.io/dataverifyr/reference/detect_backend.md)\]
  to allow user the check which backend is used

## dataverifyr 0.1.6

- allow multiline rules in yaml file
- fix bug where no fails in filter_fails would result in error
- Fix bug where multiline rules would break
- fix minor error in Readme
- [`filter_fails()`](https://davzim.github.io/dataverifyr/reference/filter_fails.md)
  allows the first argument to be a `ruleset` and not only a result of
  [`check_data()`](https://davzim.github.io/dataverifyr/reference/check_data.md)

## dataverifyr 0.1.2

- fixed NOTEs for CRAN release

## dataverifyr 0.1.0

- first stable version
- Added a `NEWS.md` file to track changes to the package.
