
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
