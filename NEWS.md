# dataverifyr 0.1.10

* Add `describe()` to describe a dataset
* `check_data()` now includes schema checks in the output by default (`check_type` as first result column), including explicit rows for column existence and declared type checks
* add `stop_on_schema_fail` to `check_data()` to optionally stop when schema checks fail
* update `filter_fails()` to ignore schema/reference rows and only process row rules from `check_data()` results
* Add explicit regression test for `detect_backend()` fallback to `dplyr` when input is a `data.frame` and `data.table` is unavailable
* Add structured ruleset internals for schema metadata (`data_column()`, `rule_meta()`) and reference checks (`reference_rule()`)
* Extend `ruleset()`, `check_data()`, `read_rules()`, and `write_rules()` for v1 schema-aware workflows; keep `rule()` as row-level API (no `col_rule()`)
* Add exported `sample_data` dataset (mixed types, NAs, datetime) for examples and tests
* export `reference_rule()` and extend examples in `ruleset()`, `check_data()`, `reference_rule()`, and `data_column()` to show combined schema + relational workflows
* Require DuckDB version `>= 1.5.2` in all DuckDB-backed tests via `skip_if_not_installed("duckdb", "1.5.2")`

# dataverifyr 0.1.9

* fix tests for new duckdb version (fixes #17, thanks @krlmlr for reporting)

# dataverifyr 0.1.8

* fix broken URL Link to pointblank

# dataverifyr 0.1.7

* rename args `fail_on_X` to `stop_on_X`
* adds `stop_on_fail` to [`check_data()`], so that the examples using `read_custom()` make sense (eg in the Readme); thanks [FedericoComoglio](https://github.com/FedericoComoglio) for pointing it out!
* fix bug when dplyr backend is used + add tests; thanks [FedericoComoglio](https://github.com/FedericoComoglio) for reporting the bug!
* expose [`detect_backend()`] to allow user the check which backend is used

# dataverifyr 0.1.6

* allow multiline rules in yaml file
* fix bug where no fails in filter_fails would result in error
* Fix bug where multiline rules would break
* fix minor error in Readme
* `filter_fails()` allows the first argument to be a `ruleset` and not only a result of `check_data()`

# dataverifyr 0.1.2

* fixed NOTEs for CRAN release

# dataverifyr 0.1.0

* first stable version
* Added a `NEWS.md` file to track changes to the package.
