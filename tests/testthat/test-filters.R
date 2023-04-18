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
})
