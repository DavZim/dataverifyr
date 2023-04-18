
check_ <- dataverifyr:::check_

data <- mtcars
data$hp[4] <- "asd"
data$disp[c(1, 5)] <- NA

rules <- ruleset(
  rule(mpg > 10, "r1"),
  rule(cyl %in% c(4, 6, 8), "r2"),
  rule(disp > 100, "r3", allow_na = TRUE), # data validation "fails"
  rule(as.numeric(hp) > 0 & as.numeric(hp) < 200, "r4", negate = TRUE), # creates warning
  rule(does_not_exist %in% c("a", "b", "c"), "r5") # creates a stop
)

test_that("base-r check_ works", {
  res <- check_(data, rules, type = "base-r")

  expect_equal(class(res), "data.frame")
  exp <- data.frame(
    name = c("r1", "r2", "r3", "r4", "r5"),
    expr = vapply(rules, function(r) r$expr, character(1)),
    tests = 32,
    pass = c(32, 32, 27, 30, 0),
    fail = c(0, 0, 5, 2, 32),
    warn = c("", "", "", "NAs introduced by coercion", ""),
    error = c("", "", "", "", "object 'does_not_exist' not found")
  )
  expect_equal(res[, setdiff(names(res), "time")], exp)
})


test_that("dplyr check_ works", {
  skip_if_not(requireNamespace("dplyr", quietly = TRUE),
              "dplyr must be installed to test the functionality")

  res <- check_(dplyr::tibble(data), rules, type = "dplyr")

  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
  exp <- dplyr::tibble(
    name = c("r1", "r2", "r3", "r4", "r5"),
    expr = vapply(rules, function(r) r$expr, character(1)),
    tests = 32,
    pass = c(32, 32, 27, 30, 0),
    fail = c(0, 0, 5, 2, 32),
    warn = c("", "", "", "NAs introduced by coercion", ""),
    error = c("", "", "", "", "object 'does_not_exist' not found")
  )
  expect_equal(res |> dplyr::select(-time), exp)
})


test_that("data.table check_ works", {
  skip_if_not(requireNamespace("data.table", quietly = TRUE),
              "data.table must be installed to test the functionality")

  res <- check_(data.table::as.data.table(data), rules, type = "data.table")

  expect_equal(class(res), c("data.table", "data.frame"))
  exp <- data.table::data.table(
    name = c("r1", "r2", "r3", "r4", "r5"),
    expr = vapply(rules, function(r) r$expr, character(1)),
    tests = 32,
    pass = c(32, 32, 27, 30, 0),
    fail = c(0, 0, 5, 2, 32),
    warn = c("", "", "", "NAs introduced by coercion", ""),
    error = c("", "", "", "", "Object 'does_not_exist' not found amongst mpg, cyl, disp, hp, drat and 6 more")
  )
  expect_equal(res[, .SD, .SDcols = !"time"], exp)
})


test_that("arrow::arrow_table check_ works", {
  skip_if_not(requireNamespace("arrow", quietly = TRUE),
              "arrow must be installed to test the functionality")

  res <- check_(arrow::arrow_table(data), rules, type = "collectibles")

  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
  exp <- dplyr::tibble(
    name = c("r1", "r2", "r3", "r4", "r5"),
    expr = vapply(rules, function(r) r$expr, character(1)),
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

  res <- check_(ds, rules, type = "collectibles")

  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
  exp <- dplyr::tibble(
    name = c("r1", "r2", "r3", "r4", "r5"),
    expr = vapply(rules, function(r) r$expr, character(1)),
    tests = 32,
    pass = c(32, 32, 27, 0, 0),
    fail = c(0, 0, 5, 32, 32),
    warn = c("", "", "", "", ""),
    error = c("", "", "", "Invalid: Failed to parse string: 'asd' as a scalar of type double", "object 'does_not_exist' not found")
  )
  expect_equal(res |> dplyr::select(-time), exp)
})


test_that("sqlite (RSQLite) check_ works", {
  skip_if_not(requireNamespace("DBI", quietly = TRUE),
              "DBI must be installed to test the functionality")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "data", data)

  tbl <- dplyr::tbl(con, "data")

  res <- check_(tbl, rules, type = "collectibles")

  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
  exp <- dplyr::tibble(
    name = c("r1", "r2", "r3", "r4", "r5"),
    expr = vapply(rules, function(r) r$expr, character(1)),
    tests = 32,
    pass = c(32, 32, 27, 30, 0),
    fail = c(0, 0, 5, 2, 32),
    warn = c("", "", "", "", ""), # note that DBI(?) silently converts the asd hp value to 0!
    # c.f. https://stackoverflow.com/a/57746647/3048453
    error = c("", "", "", "", "no such column: does_not_exist")
  )
  expect_equal(res |> dplyr::select(-time), exp)
})
