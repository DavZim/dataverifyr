
check_ <- dataverifyr:::check_

data <- mtcars
data$hp[4] <- "asd"
data$disp[c(1, 5)] <- NA

rules <- ruleset(
  rule(cyl %in% c(4, 6, 8), "r1"),
  rule(mpg < 10 & mpg > 34, "r2", negate = TRUE),
  rule(disp > 100, "r3", allow_na = TRUE), # data validation "fails"
  rule(as.numeric(hp) > 0 & as.numeric(hp) < 400, "r4"), # creates warning + 1 NA
  rule(does_not_exist %in% c("a", "b", "c"), "r5") # creates a stop
)

test_that("base-r check_ works", {
  res <- check_(data, rules, backend = "base-r")

  expect_equal(class(res), "data.frame")
  exp <- data.frame(
    name = c("r1", "r2", "r3", "r4", "r5"),
    expr = vapply(rules, function(r) r$expr, character(1)),
    allow_na = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    negate = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    tests = 32,
    pass = c(32, 32, 27, 31, 0),
    fail = c(0, 0, 5, 1, 32),
    warn = c("", "", "", "NAs introduced by coercion", ""),
    error = c("", "", "", "", "object 'does_not_exist' not found")
  )
  expect_equal(res[, setdiff(names(res), "time")], exp)
})


test_that("dplyr check_ works", {
  skip_if_not(requireNamespace("dplyr", quietly = TRUE),
              "dplyr must be installed to test the functionality")

  res <- check_(dplyr::tibble(data), rules, backend = "dplyr")

  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
  exp <- dplyr::tibble(
    name = c("r1", "r2", "r3", "r4", "r5"),
    expr = vapply(rules, function(r) r$expr, character(1)),
    allow_na = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    negate = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    tests = 32,
    pass = c(32, 32, 27, 31, 0),
    fail = c(0, 0, 5, 1, 32),
    warn = c("", "", "", "NAs introduced by coercion", ""),
    error = c("", "", "", "", "object 'does_not_exist' not found")
  )
  expect_equal(res |> dplyr::select(-time), exp)
})


test_that("data.table check_ works", {
  skip_if_not(requireNamespace("data.table", quietly = TRUE),
              "data.table must be installed to test the functionality")

  res <- check_(data.table::as.data.table(data), rules, backend = "data.table")

  expect_equal(class(res), c("data.table", "data.frame"))
  exp <- data.table::data.table(
    name = c("r1", "r2", "r3", "r4", "r5"),
    expr = vapply(rules, function(r) r$expr, character(1)),
    allow_na = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    negate = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    tests = 32,
    pass = c(32, 32, 27, 31, 0),
    fail = c(0, 0, 5, 1, 32),
    warn = c("", "", "", "NAs introduced by coercion", "")
  )
  expect_equal(res[, .SD, .SDcols = names(exp)], exp)

  # use regex to make test for missing column work for new version of data.table
  # c.f. https://github.com/DavZim/dataverifyr/issues/3
  errors <- c("", "", "", "",
              "Object 'does_not_exist' not found amongst \\[?mpg, cyl, disp, hp, drat")
  expect_true(all(mapply(grepl, pattern = errors, x = res$error)))
})


test_that("arrow::arrow_table check_ works", {
  skip_if_not(requireNamespace("arrow", quietly = TRUE),
              "arrow must be installed to test the functionality")

  res <- check_(arrow::arrow_table(data), rules, backend = "collectibles")

  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
  exp <- dplyr::tibble(
    name = c("r1", "r2", "r3", "r4", "r5"),
    expr = vapply(rules, function(r) r$expr, character(1)),
    allow_na = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    negate = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    tests = 32,
    pass = c(32, 32, 27, 0, 0),
    fail = c(0, 0, 5, 32, 32),
    warn = c("", "", "", "", ""),
    error = c("", "", "", "Invalid: Failed to parse string: 'asd' as a scalar of type double", "object 'does_not_exist' not found")
  )
  expect_equal(res |> dplyr::select(-time), exp)
})


test_that("arrow::open_dataset check_ works", {
  skip_if_not(requireNamespace("arrow", quietly = TRUE),
              "arrow must be installed to test the functionality")

  temp <- file.path(tempdir(), "data")
  arrow::write_dataset(data, temp)
  ds <- arrow::open_dataset(temp)

  res <- check_(ds, rules, backend = "collectibles")

  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
  exp <- dplyr::tibble(
    name = c("r1", "r2", "r3", "r4", "r5"),
    expr = vapply(rules, function(r) r$expr, character(1)),
    allow_na = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    negate = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    tests = 32,
    pass = c(32, 32, 27, 0, 0),
    fail = c(0, 0, 5, 32, 32),
    warn = c("", "", "", "", ""),
    error = c("", "", "", "Invalid: Failed to parse string: 'asd' as a scalar of type double", "object 'does_not_exist' not found")
  )
  expect_equal(res |> dplyr::select(-time), exp)
})


test_that("sqlite (RSQLite) check_ works", {
  skip_if_not(requireNamespace("DBI", quietly = TRUE) |
                requireNamespace("dbplyr", quietly = TRUE) |
                requireNamespace("RSQLite", quietly = TRUE),
              "DBI, dbplyr, and RSQLite must be installed to test the functionality")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "data", data)

  tbl <- dplyr::tbl(con, "data")

  res <- check_(tbl, rules, backend = "collectibles")

  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
  exp <- dplyr::tibble(
    name = c("r1", "r2", "r3", "r4", "r5"),
    expr = vapply(rules, function(r) r$expr, character(1)),
    allow_na = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    negate = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    tests = 32,
    pass = c(32, 32, 27, 31, 0),
    fail = c(0, 0, 5, 1, 32),
    warn = ""
    # note that RSQLite silently converts the 'asd' hp value to 0!
    # c.f. https://stackoverflow.com/a/57746647/3048453
  )
  expect_equal(res |> dplyr::select(-time, -error), exp)
  # the error messages are unreliable as the wording changes over versions,
  # test that there is some error message
  expect_equal(nchar(res$error) > 0, c(FALSE, FALSE, FALSE, FALSE, TRUE))
})


test_that("duckdb check_ works", {
  skip_if_not(requireNamespace("DBI", quietly = TRUE) |
                requireNamespace("dbplyr", quietly = TRUE) |
                requireNamespace("duckdb", quietly = TRUE),
              "DBI, dplyr, and duckdb must be installed to test the functionality")

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "data", data)

  tbl <- dplyr::tbl(con, "data")

  res <- check_(tbl, rules, backend = "collectibles")

  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
  exp <- dplyr::tibble(
    name = c("r1", "r2", "r3", "r4", "r5"),
    expr = vapply(rules, function(r) r$expr, character(1)),
    allow_na = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    negate = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    tests = 32,
    pass = c(32, 32, 27, 0, 0),
    fail = c(0, 0, 5, 32, 32),
    warn = ""
  )
  expect_equal(res |> dplyr::select(-time, -error), exp)
  # the error messages are unreliable as the wording changes over versions,
  # test that there is some error message
  expect_equal(nchar(res$error) > 0, c(FALSE, FALSE, FALSE, TRUE, TRUE))
})


test_that("Test extra functionality", {
  # works with only one rule as opposed to a ruleset =====
  res <- check_data(data, rules[[1]])
  res2 <- check_data(data, ruleset(rules[[1]]))
  expect_equal(res[, !"time"], res2[, !"time"])

  # works with rules as a file =====
  rule_file <- "temp-rules.yml"
  write_rules(rules, rule_file)

  res <- check_data(data, rule_file)
  res2 <- check_data(data, rules)
  expect_equal(res[, !"time"], res2[, !"time"])
  unlink(rule_file)

  # fails on warn and fail on error work =====
  expect_error(check_data(data, rules, fail_on_warn = TRUE))
  expect_error(check_data(data, rules, fail_on_error = TRUE))
})
