test_that("rule spec constructors validate inputs", {
  expect_false(exists("col_rule", where = asNamespace("dataverifyr"), inherits = FALSE))
  expect_true(exists("data_column", where = asNamespace("dataverifyr"), inherits = FALSE))
  expect_true(exists("reference_rule", where = asNamespace("dataverifyr"), inherits = FALSE))

  expect_error(data_column(col = 1), "character")
  expect_error(data_column(col = "a", type = 1), "character")
  expect_error(data_column(col = "a", optional = "yes"), "logical")

  expect_error(
    ruleset(
      rule(a > 0),
      data_columns = list(
        data_column("a"),
        data_column("a")
      )
    ),
    "duplicate"
  )
})


test_that("write_rules/read_rules support structured v1 yaml", {
  rs <- ruleset(
    rule(a > 0, name = "positive a"),
    data_columns = list(
      data_column("a", type = "int", optional = FALSE, description = "A column")
    ),
    meta = dataverifyr:::rule_meta(title = "demo", version = "1.0", description = "desc")
  )

  file <- tempfile(fileext = ".yml")
  write_rules(rs, file, format = "v1")

  yaml_lines <- readLines(file)
  expect_true(any(grepl("^meta:$", yaml_lines)))
  expect_true(any(grepl("^data-columns:$", yaml_lines)))
  expect_true(any(grepl("^data-rules:$", yaml_lines)))

  rs2 <- read_rules(file)
  expect_s3_class(rs2, "ruleset")
  expect_equal(rs2[[1]]$name, "positive a")
  expect_equal(attr(rs2, "meta")$title, "demo")
  expect_equal(attr(rs2, "data_columns")[[1]]$col, "a")
})


test_that("read_rules rejects malformed v1 yaml", {
  file <- tempfile(fileext = ".yml")
  writeLines(c(
    "meta:",
    "  title: bad",
    "data-columns:",
    "  - col: a"
  ), file)

  expect_error(read_rules(file), "data-rules")
})


test_that("check_data validates schema and supports extra_columns modes", {
  rs <- ruleset(
    rule(a > 0),
    data_columns = list(
      data_column("a", type = "int", optional = FALSE),
      data_column("b", type = "int", optional = TRUE)
    )
  )

  x <- data.frame(a = 1:3, b = 1:3, c = 9:11)

  res_ignore <- check_data(x, rs, extra_columns = "ignore")
  expect_true("check_type" %in% names(res_ignore))
  expect_equal(names(res_ignore)[[1]], "check_type")
  expect_true(all(res_ignore$fail == 0))
  expect_true(any(res_ignore$check_type == "schema"))
  expect_true(any(res_ignore$check_type == "row_rule"))

  expect_warning(check_data(x, rs, extra_columns = "warn"), "extra")
  expect_error(check_data(x, rs, extra_columns = "fail"), "extra")

  res_missing <- check_data(data.frame(b = 1:2), rs)
  expect_true(any(
    res_missing$check_type == "schema" &
      grepl("exists", res_missing$name) &
      res_missing$fail == 1
  ))
  expect_error(
    check_data(data.frame(b = 1:2), rs, stop_on_schema_fail = TRUE),
    "schema fails"
  )

  rs_unknown <- ruleset(
    rule(does_not_exist > 0),
    data_columns = list(data_column("a"))
  )
  expect_error(check_data(data.frame(a = 1:3), rs_unknown), "Unknown symbols")

  rs_type <- ruleset(
    rule(a %in% c("x", "y")),
    data_columns = list(data_column("a", type = "int"))
  )
  res_type <- check_data(data.frame(a = c("x", "y")), rs_type)
  expect_true(any(
    res_type$check_type == "schema" &
      grepl("type", res_type$name) &
      res_type$fail == 1
  ))
  expect_error(
    check_data(data.frame(a = c("x", "y")), rs_type, stop_on_schema_fail = TRUE),
    "schema fails"
  )
})

test_that("check_data supports ruleset with only data_columns", {
  rs <- ruleset(
    data_columns = list(
      data_column("a", type = "int", optional = FALSE),
      data_column("b", type = "str", optional = TRUE)
    )
  )

  res <- check_data(data.frame(a = 1:3), rs)
  expect_true(nrow(res) > 0)
  expect_true(all(res$check_type == "schema"))
  expect_false(any(res$check_type == "row_rule"))
  expect_true(all(res$fail == 0))
})


test_that("check_data supports cross-reference rules across datasets", {
  flights <- data.frame(carrier = c("AA", "BB", NA_character_))
  carriers <- data.frame(carrier_id = c("AA"))

  rs <- ruleset(
    reference_rule(
      local_col = "carrier",
      ref_dataset = "carriers",
      ref_col = "carrier_id",
      name = "carrier in carriers",
      allow_na = TRUE
    ),
    data_name = "flights"
  )

  res <- check_data(list(flights = flights, carriers = carriers), rs)
  expect_equal(res$name, "carrier in carriers")
  expect_equal(res$pass, 2)
  expect_equal(res$fail, 1)

  expect_error(
    check_data(list(flights = flights), rs),
    "referenced dataset"
  )
})
