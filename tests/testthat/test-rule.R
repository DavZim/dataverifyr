test_that("Basic rules work", {

  # basic version of rule
  expect_equal(unclass(rule(mpg > 10)),
               list(name = "Rule for: mpg", expr = "mpg > 10", allow_na = FALSE,
                    negate = FALSE))
  # expression can be given as a string as well
  expect_equal(unclass(rule("mpg > 10")),
               list(name = "Rule for: mpg", expr = "mpg > 10", allow_na = FALSE,
                    negate = FALSE))

  # additional information is carried along as well
  expect_equal(unclass(rule(mpg > 10, author = "me", date = Sys.Date())),
               list(name = "Rule for: mpg", expr = "mpg > 10", allow_na = FALSE,
                    negate = FALSE, author = "me", date = Sys.Date()))
})


test_that("get_symbols works as expected", {
  f <- function(x) deparse(substitute(x))
  get_symbols <- dataverifyr:::get_symbols

  expect_equal(get_symbols(f(mpg > 10)), "mpg")

  expect_equal(
    get_symbols(f(mpg > 10 & mpg <= 123 | is.na(cyl) & as.numeric(wt) > qsec)),
    c("mpg", "cyl", "wt", "qsec")
  )
})
