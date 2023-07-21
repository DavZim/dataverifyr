test_that("filters return values that are not matched by the rules", {
  data <- mtcars

  data$mpg[4] <- NA
  data$cyl[5] <- NA

  rules <- ruleset(
    rule(mpg > 10 & mpg < 30, allow_na = TRUE), # mpg goes up to 34
    rule(cyl != 6, negate = TRUE), # there are 6 cyl
    rule(vs %in% c(0, 1))
  )

  res <- check_data(data, rules)

  fails <- filter_fails(res, data, per_rule = TRUE)

  # same number of values returned as fails reported
  expect_equal(unname(vapply(fails, nrow, integer(1))),
               res$fail[res$fail != 0])


  # check values of fails
  exp <- list(
    with(data, data[!((mpg > 10 & mpg < 30) | is.na(mpg)), ]),
    with(data, data[!(cyl == 6 & !is.na(cyl)), ]) # negated rule...
  )
  exp <- lapply(exp, function(e) {
    rownames(e) <- NULL
    e
  })

  expect_equal(lapply(unname(fails), as.data.frame), exp)



  # same result when rules are supplied to check_data
  fails2 <- filter_fails(rules, data, per_rule = TRUE)
  expect_equal(fails2, fails)
})

test_that("No fails do not crash", {
  data <- mtcars

  rules <- ruleset(
    rule(mpg > 10 & mpg < 35),
    rule(cyl != 5),
    rule(vs %in% c(0, 1))
  )

  res <- check_data(data, rules)
  rr <- filter_fails(res, data)
  expect_equal(nrow(rr), 0)
  expect_equal(names(rr), names(data))

  # also works when ruleset is provided
  rr2 <- filter_fails(rules, data)
  expect_equal(nrow(rr2), 0)
  expect_equal(names(rr2), names(data))
})
