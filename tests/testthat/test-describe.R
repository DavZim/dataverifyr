# create a sample dataset to describe
set.seed(41)
data <- data.frame(
  # numeric
  a = as.numeric(seq(20)),
  b = rnorm(20),
  # integer
  c = as.integer(seq(20)),
  d = sample.int(20, 10, replace = TRUE),
  # character
  e = sample(letters, 20, replace = TRUE),
  f = sample(c(letters, paste0(letters, letters), paste0(letters, letters, letters)),
             20, replace = TRUE),
  # factor
  g = factor(sample(letters, 20, replace = TRUE)),
  # posixct
  h = as.POSIXct(
    sample.int(365*60*60*24, 10, replace = TRUE),
    origin = "2020-01-01",
    tz = "UTC"
  )
)

for (n in names(data)) data[sample.int(nrow(data), 3), n] <- NA

normalize_mf <- function(x) {
  vapply(strsplit(x, ", ", fixed = TRUE), function(parts) {
    paste(sort(parts), collapse = ", ")
  }, character(1))
}


test_that("describe data.frame", {
  local_mocked_bindings(has_pkg = function(p) p %in% pkgs, .package = "dataverifyr")
  pkgs <- NULL

  expect_equal(detect_backend(data), "base-r")
  d <- describe(data, skip_ones = FALSE)
  expect_equal(class(d), "data.frame")
  exp <- d

  skip_if_not(
    requireNamespace("dplyr", quietly = TRUE) &&
      requireNamespace("data.table", quietly = TRUE),
    "dplyr and data.table must be installed to run these tests"
  )

  # use data.table
  pkgs <- "data.table"
  expect_equal(detect_backend(data), "data.table")
  d <- describe(data, skip_ones = FALSE)
  expect_equal(class(d), c("data.table", "data.frame"))
  expect_equal(as.data.frame(d)[setdiff(names(d), "most_frequent")], exp[setdiff(names(exp), "most_frequent")])
  expect_equal(normalize_mf(as.data.frame(d)$most_frequent), normalize_mf(exp$most_frequent))

  # use dplyr
  pkgs <- "dplyr"
  expect_equal(detect_backend(data), "dplyr")
  d <- describe(data, skip_ones = FALSE)
  expect_equal(class(d), c("tbl_df", "tbl", "data.frame"))
  expect_equal(as.data.frame(d)[setdiff(names(d), "most_frequent")], exp[setdiff(names(exp), "most_frequent")])
  expect_equal(normalize_mf(as.data.frame(d)$most_frequent), normalize_mf(exp$most_frequent))
})


test_that("describe sqlite", {
  skip_if_not(
    requireNamespace("DBI", quietly = TRUE) &&
      requireNamespace("dplyr", quietly = TRUE) &&
      requireNamespace("dbplyr", quietly = TRUE) &&
      requireNamespace("RSQLite", quietly = TRUE),
    "DBI, dplyr, dbplyr, and RSQLite must be installed to test the functionality"
  )

  con <- DBI::dbConnect(RSQLite::SQLite())
  DBI::dbWriteTable(con, "data", data)
  x <- dplyr::tbl(con, "data")
  expect_equal(detect_backend(x), "collectibles")

  d <- describe(x, skip_ones = FALSE)

  expect_equal(class(d), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(d), ncol(data))
  expect_true(all(c("var", "type", "n", "n_distinct", "n_na", "most_frequent") %in% names(d)))

  DBI::dbDisconnect(con)
})


test_that("describe duckdb", {
  skip_if_not(
    requireNamespace("DBI", quietly = TRUE) &&
      requireNamespace("dplyr", quietly = TRUE) &&
      requireNamespace("dbplyr", quietly = TRUE) &&
      requireNamespace("duckdb", quietly = TRUE),
    "DBI, dplyr, dbplyr, and duckdb must be installed to test the functionality"
  )

  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbWriteTable(con, "data", data)
  x <- dplyr::tbl(con, "data")
  expect_equal(detect_backend(x), "collectibles")

  d <- describe(x, skip_ones = FALSE)

  expect_equal(class(d), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(d), ncol(data))
  expect_true(all(c("var", "type", "n", "n_distinct", "n_na", "most_frequent") %in% names(d)))
  DBI::dbDisconnect(con)

})


test_that("describe arrow", {
  skip_if_not(
    requireNamespace("dbplyr", quietly = TRUE) &&
      requireNamespace("arrow", quietly = TRUE),
    "dplyr and arrow must be installed to test the functionality"
  )

  tmp <- tempfile()
  arrow::write_parquet(data, tmp)
  x <- arrow::open_dataset(tmp)
  expect_equal(detect_backend(x), "collectibles")

  d <- describe(x, skip_ones = FALSE)

  expect_equal(class(d), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(d), ncol(data))
  expect_true(all(c("var", "type", "n", "n_distinct", "n_na", "most_frequent") %in% names(d)))
})


test_that("describe most_frequent rounds numeric values and suppresses <=1 distinct", {
  local_mocked_bindings(has_pkg = function(...) FALSE, .package = "dataverifyr")

  x <- data.frame(
    one_value = rep(3.1415926535, 5),
    all_na = rep(NA_real_, 5),
    mixed = c(1.23456789, 1.23456789, 9.87654321, NA_real_, 9.87654321)
  )

  d <- suppressWarnings(describe(x, skip_ones = FALSE))

  expect_equal(d$most_frequent[d$var == "one_value"], "")
  expect_equal(d$most_frequent[d$var == "all_na"], "")
  expect_equal(
    d$most_frequent[d$var == "mixed"],
    "1.2346 (2), 9.8765 (2), NA (1)"
  )
})


test_that("describe skips one-count values in most_frequent by default", {
  local_mocked_bindings(has_pkg = function(...) FALSE, .package = "dataverifyr")

  x <- data.frame(
    mostly_unique = c(1, 2, 3, 4, 5),
    repeated = c(1, 1, 2, 3, NA_real_)
  )

  d <- describe(x)
  expect_equal(d$most_frequent[d$var == "mostly_unique"], "")
  expect_equal(d$most_frequent[d$var == "repeated"], "1 (2)")

  d_keep_ones <- describe(x, skip_ones = FALSE)
  expect_equal(d_keep_ones$most_frequent[d_keep_ones$var == "repeated"], "1 (2), 2 (1), 3 (1)")
})


test_that("describe supports configurable digits for most_frequent numeric rounding", {
  local_mocked_bindings(has_pkg = function(...) FALSE, .package = "dataverifyr")

  x <- data.frame(
    mixed = c(1.23456789, 1.23456789, 9.87654321, 9.87654321, NA_real_)
  )

  d <- describe(x, skip_ones = FALSE, digits = 2)
  expect_equal(d$most_frequent[d$var == "mixed"], "1.23 (2), 9.88 (2), NA (1)")
})

test_that("describe dplyr backend supports skip_ones and digits", {
  skip_if_not(requireNamespace("dplyr", quietly = TRUE),
              "dplyr must be installed to run this test")
  local_mocked_bindings(has_pkg = function(p) p == "dplyr", .package = "dataverifyr")

  x <- data.frame(
    num = c(1.234567, 1.234567, 9.876543, 9.876543, NA_real_),
    chr = c("a", "a", "b", "c", NA_character_)
  )

  d <- describe(x, skip_ones = FALSE, digits = 2)
  expect_equal(detect_backend(x), "dplyr")
  expect_equal(class(d), c("tbl_df", "tbl", "data.frame"))
  expect_equal(d$most_frequent[d$var == "num"], "1.23 (2), 9.88 (2), NA (1)")
  expect_equal(d$most_frequent[d$var == "chr"], "a (2), b (1), c (1)")

  d2 <- describe(x, skip_ones = TRUE, digits = 2)
  expect_equal(d2$most_frequent[d2$var == "num"], "1.23 (2), 9.88 (2)")
  expect_equal(d2$most_frequent[d2$var == "chr"], "a (2)")
})

test_that("describe data.table backend supports skip_ones and digits", {
  skip_if_not(requireNamespace("data.table", quietly = TRUE),
              "data.table must be installed to run this test")
  local_mocked_bindings(has_pkg = function(p) p == "data.table", .package = "dataverifyr")

  x <- data.frame(
    num = c(1.234567, 1.234567, 9.876543, 9.876543, NA_real_),
    chr = c("a", "a", "b", "c", NA_character_)
  )

  d <- describe(x, skip_ones = FALSE, digits = 2)
  expect_equal(detect_backend(x), "data.table")
  expect_equal(class(d), c("data.table", "data.frame"))
  expect_equal(d$most_frequent[d$var == "num"], "1.23 (2), 9.88 (2), NA (1)")
  expect_equal(d$most_frequent[d$var == "chr"], "a (2), b (1), c (1)")

  d2 <- describe(x, skip_ones = TRUE, digits = 2)
  expect_equal(d2$most_frequent[d2$var == "num"], "1.23 (2), 9.88 (2)")
  expect_equal(d2$most_frequent[d2$var == "chr"], "a (2)")
})
