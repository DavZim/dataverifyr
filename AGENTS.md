# Dataverifyr

An R Library to verify that data exists and is valid, see comprehensive README.md.

All code must be thoroughly tested and follow modern R standards.
Test first: Before writing any code write the expected tests!

All material updates to the code base itself must be registered in the NEWS.md file.

Never change .Rd files, instead change the roxygen2 documentation above the function (use `devtools::document()` to create the .Rd documentation).

The package is build with efficiency in mind.
This shows for example that most operations are pushed to the database layer if possible.
Also, additional dependencies need to be explicity allowed before they are added!

When invoking R, use `/opt/R/4.4.0/bin/R` or `/opt/R/4.4.0/bin/Rscript`.

To check, use `rcmdcheck::rcmdcheck()`.

