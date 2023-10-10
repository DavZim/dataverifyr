library(testthat)
library(dataverifyr)

test_that("detect_backend works as expected", {
  data <- mtcars

  # with no other packages installed, use base-r
  pkgs <- NULL
  local_mocked_bindings(has_pkg = function(p) p %in% pkgs, .package = "dataverifyr")
  expect_equal(detect_backend(data), "base-r")

  # use data.table if installed
  pkgs <- "data.table"
  expect_equal(detect_backend(data), "data.table")

  # use dplyr if no data.table is installed
  pkgs <- "dplyr"
  expect_equal(detect_backend(data), "dplyr")

  # use data.table if both dplyr and data.table are found
  pkgs <- c("dplyr", "data.table")
  expect_equal(detect_backend(data), "data.table")


  # test dplyr dataset
  class(data) <- c("tbl_df", "tbl", "data.frame")
  pkgs <- NULL
  expect_error(detect_backend(data), "The dplyr package needs to be installed")

  # if tibble is given but no dplyr is installed -> error
  pkgs <- "data.table"
  expect_error(detect_backend(data), "The dplyr package needs to be installed")

  pkgs <- "dplyr"
  expect_equal(detect_backend(data), "dplyr")


  # test data.table dataset
  class(data) <- c("data.table", "data.frame")
  pkgs <- NULL
  expect_error(detect_backend(data), "The data.table package needs to be installed")

  # if data.table is given but no data.table is installed -> error
  pkgs <- "dplyr"
  expect_error(detect_backend(data), "The data.table package needs to be installed")

  pkgs <- "data.table"
  expect_equal(detect_backend(data), "data.table")


  # test arrow dataset
  class(data) <- c("FileSystemDataset", "Dataset", "ArrowObject", "R6")
  pkgs <- NULL
  expect_error(detect_backend(data), "The arrow package needs to be installed")

  pkgs <- "arrow"
  expect_equal(detect_backend(data), "collectibles")


  # test DBI dataset
  class(data) <- c("tbl_SQLiteConnection", "tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")
  pkgs <- NULL
  expect_error(detect_backend(data), "The DBI package needs to be installed")

  pkgs <- "DBI"
  expect_equal(detect_backend(data), "collectibles")
})
