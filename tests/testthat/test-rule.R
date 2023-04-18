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
