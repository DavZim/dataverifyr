
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
  h = as.POSIXct(sample.int(365*60*60*24, 10, replace = TRUE), origin = "2020-01-01")
)
for (n in names(data))
  data[sample.int(nrow(data), 3), n] <- NA

# the expected values
exp <- data.frame(
  var = c("a", "b", "c", "d", "e", "f", "g", "h"),
  type = c("numeric", "numeric", "integer", "integer", "character",
           "character", "factor", "POSIXct"),
  n = c(20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L),
  n_distinct = c(18L, 18L, 18L, 8L, 14L, 15L, 13L, 11L),
  n_na = c(3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L),
  most_frequent = c("NA (3), 1 (1), 2 (1)",
                    "NA (3), -0.794368337075183 (1), 0.197257539137862 (1)",
                    "NA (3), 1 (1), 2 (1)", "16 (4), NA (3), 13 (3)",
                    "u (3), NA (3), r (2)", "NA (3), ggg (3), x (2)",
                    "g (3), NA (3), b (2)",
                    "NA (3), 2020-11-16 01:27:37 (2), 2020-08-10 23:06:51 (2)"),
  min = c(1, -1.57960700723589, 1, 7, 1, 1, 1, 1578213277),
  mean = c(9.82352941176471, 0.395917134682771, 9.70588235294118,
           13.4705882352941, 1, 2.11764705882353, 1, 1597372702.64706),
  median = c(10, 0.493667470676325, 10, 13, 1, 2, 1, 1601657240),
  max = c(20, 2.27440247098253, 20, 18, 1, 3, 1, 1606208141),
  sd = c(5.92911559717854, 1.08100890056568, 5.7961701351232,
         3.6420743927538, 0, 0.857492925712544, 0, 9824874.35812236)
)

test_that("describe data.frame", {
  local_mocked_bindings(has_pkg = function(p) p %in% pkgs, .package = "dataverifyr")
  pkgs <- NULL
  expect_equal(detect_backend(data), "base-r")
  d <- describe(data)
  expect_equal(class(d), "data.frame")
  expect_equal(d, exp)

  skip_if_not(requireNamespace("dplyr", quietly = TRUE) |
                requireNamespace("data.table", quietly = TRUE),
              "dplyr and data.table must be installed to run these tests")

  # use data.table
  pkgs <- "data.table"
  expect_equal(detect_backend(data), "data.table")
  d <- describe(data)
  expect_equal(class(d), c("data.table", "data.frame"))
  expect_equal(as.data.frame(d), exp)

  # use dplyr
  pkgs <- "dplyr"
  expect_equal(detect_backend(data), "dplyr")
  d <- describe(data)
  expect_equal(class(d), c("tbl_df", "tbl", "data.frame"))
  expect_equal(as.data.frame(d), exp)
  # error here as dplyr backend sorts the data slightly differently...
})

test_that("sqlite", {
  skip_if_not(requireNamespace("DBI", quietly = TRUE) |
                requireNamespace("dplyr", quietly = TRUE) |
                requireNamespace("dbplyr", quietly = TRUE) |
                requireNamespace("RSQLite", quietly = TRUE),
              "DBI, dplyr, dbplyr, and RSQLite must be installed to test the functionality")

  con <- DBI::dbConnect(RSQLite::SQLite())
  DBI::dbWriteTable(con, "data", data)
  x <- dplyr::tbl(con, "data")
  expect_equal(detect_backend(x), "collectibles")

  d <- describe(x)
  # this takes waaaaay too long....

  expect_equal(class(d), c("tbl_df", "tbl", "data.frame"))
  expect_equal(as.data.frame(d), exp)
  # error here: different order and missing last row due to posixct

  DBI::dbDisconnect(con)
})

test_that("duckdb", {
  skip_if_not(requireNamespace("DBI", quietly = TRUE) |
                requireNamespace("dplyr", quietly = TRUE) |
                requireNamespace("dbplyr", quietly = TRUE) |
                requireNamespace("duckdb", quietly = TRUE),
              "DBI, dplyr, dbplyr, and duckdb must be installed to test the functionality")

  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbWriteTable(con, "data", data)
  x <- dplyr::tbl(con, "data")
  expect_equal(detect_backend(x), "collectibles")

  d <- describe(x)
  # this takes waaaaay too long....

  expect_equal(class(d), c("tbl_df", "tbl", "data.frame"))
  expect_equal(as.data.frame(d), exp)
  # error here: different order and missing last row due to posixct
  DBI::dbDisconnect(con)

})
test_that("arrow", {
  skip_if_not(requireNamespace("dbplyr", quietly = TRUE) |
                requireNamespace("arrow", quietly = TRUE),
              "dplyr and arrow must be installed to test the functionality")

  tmp <- tempfile()
  arrow::write_parquet(data, tmp)
  x <- arrow::open_dataset(tmp)
  expect_equal(detect_backend(x), "collectibles")

  d <- describe(x)
  # this takes waaaaay too long....

  expect_equal(class(d), c("tbl_df", "tbl", "data.frame"))
  expect_equal(as.data.frame(d), exp)
  # error here: different order and missing last row due to posixct
})
