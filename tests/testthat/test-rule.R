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

  # rules can span multiple lines and do not throw a warning!
  r <- rule(mpg > 10 &
              cyl %in% c(4, 6, 8) |
              disp > 10)
  expect_equal(r$expr, "mpg > 10 & cyl %in% c(4, 6, 8) | disp > 10")
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

test_that("print.rule prints expected structure", {
  r <- rule(
    mpg > 10,
    name = "mpg rule",
    allow_na = TRUE,
    negate = TRUE,
    author = "qa"
  )

  out <- paste(capture.output(print(r)), collapse = "\n")

  expect_match(out, "<Verification Rule>")
  expect_match(out, "expr: 'mpg > 10'")
  expect_match(out, "name: 'mpg rule'")
  expect_match(out, "allow NA: TRUE")
  expect_match(out, "negated:  TRUE")
  expect_match(out, "author: 'qa'")
  ret <- NULL
  capture.output(ret <- withVisible(print(r)))
  expect_false(ret$visible)
  expect_identical(ret$value, r)
})

test_that("print.ruleset respects n and truncation message", {
  rs <- ruleset(
    rule(mpg > 10, name = "mpg"),
    rule(cyl %in% c(4, 6, 8), name = "cyl", allow_na = TRUE),
    rule(qsec > 14, name = "qsec", negate = TRUE),
    rule(hp < 300, name = "hp")
  )

  out_short <- paste(capture.output(print(rs, n = 3)), collapse = "\n")
  expect_match(out_short, "<Verification Ruleset with 4 elements>")
  expect_match(out_short, "\\[1\\] 'mpg' matching `mpg > 10` \\(allow_na: FALSE\\)")
  expect_match(out_short, "\\[2\\] 'cyl' matching `cyl %in% c\\(4, 6, 8\\)` \\(allow_na: TRUE\\)")
  expect_match(out_short, "\\[3\\] 'qsec' matching `qsec > 14` \\(allow_na: FALSE, negated\\)")
  expect_match(out_short, "\\.\\.\\. \\+1 more\\. Use print\\(ruleset, n = 10\\) to print more\\.")

  out_long <- paste(capture.output(print(rs, n = 10)), collapse = "\n")
  expect_match(out_long, "\\[4\\] 'hp' matching `hp < 300` \\(allow_na: FALSE\\)")
  expect_no_match(out_long, "\\.\\.\\. \\+")
  ret <- NULL
  capture.output(ret <- withVisible(print(rs)))
  expect_false(ret$visible)
  expect_identical(ret$value, rs)
})
