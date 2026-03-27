test_that("sample_data is available and has documented structure", {
  expect_true(exists("sample_data", where = asNamespace("dataverifyr"), inherits = FALSE))

  x <- get("sample_data", envir = asNamespace("dataverifyr"))
  expect_s3_class(x, "data.frame")

  expect_equal(
    names(x),
    c("order_id", "customer_tier", "amount", "paid", "payment_method", "order_time")
  )

  expect_true(is.integer(x$order_id))
  expect_true(is.character(x$customer_tier))
  expect_true(is.numeric(x$amount))
  expect_true(is.logical(x$paid))
  expect_true(is.character(x$payment_method))
  expect_s3_class(x$order_time, "POSIXct")

  expect_true(any(is.na(x$customer_tier)))
  expect_true(any(is.na(x$amount)))
  expect_true(any(is.na(x$paid)))
  expect_true(any(is.na(x$payment_method)))
  expect_true(any(is.na(x$order_time)))
})


test_that("sample_data can be used for row and column checks", {
  rs <- ruleset(
    rule(amount >= 0, name = "amount must be non-negative", allow_na = TRUE),
    rule(!paid | payment_method != "none", name = "paid orders require payment", allow_na = TRUE),
    data_columns = list(
      data_column("order_id", type = "int", optional = FALSE),
      data_column("customer_tier", type = "str", optional = FALSE),
      data_column("amount", type = "double", optional = FALSE),
      data_column("paid", type = "logical", optional = FALSE),
      data_column("payment_method", type = "str", optional = FALSE),
      data_column("order_time", type = "str", optional = TRUE)
    )
  )

  x <- get("sample_data", envir = asNamespace("dataverifyr"))

  res_type <- check_data(x, rs)
  expect_true(any(
    res_type$check_type == "schema" &
      grepl("type", res_type$name) &
      res_type$fail == 1
  ))
  expect_error(check_data(x, rs, stop_on_schema_fail = TRUE), "schema fails")

  rs_ok <- ruleset(
    rule(amount >= 0, name = "amount must be non-negative", allow_na = TRUE),
    data_columns = list(
      data_column("order_id", type = "int", optional = FALSE),
      data_column("customer_tier", type = "str", optional = FALSE),
      data_column("amount", type = "double", optional = FALSE),
      data_column("paid", type = "logical", optional = FALSE),
      data_column("payment_method", type = "str", optional = FALSE),
      data_column("order_time", optional = TRUE)
    )
  )

  res <- check_data(x, rs_ok, extra_columns = "ignore")
  expect_s3_class(res, "data.frame")
  expect_true(all(c("name", "pass", "fail") %in% names(res)))
})
