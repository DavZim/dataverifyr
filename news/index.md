# Changelog

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
