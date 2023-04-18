test_that("Basic im and export works", {
  rr <- ruleset(
    rule(mpg > 10),
    rule(cyl %in% c(4, 6, 8))
  )
  file <- tempfile(fileext = ".yml")
  write_rules(rr, file)

  expect_equal(readLines(file),
               c("- name: 'Rule for: mpg'",
                 "  expr: mpg > 10",
                 "  allow_na: no",
                 "  negate: no",
                 "  index: 1",
                 "- name: 'Rule for: cyl'",
                 "  expr: cyl %in% c(4, 6, 8)",
                 "  allow_na: no",
                 "  negate: no",
                 "  index: 2"))

  rr2 <- read_rules(file)
  expect_equal(rr, rr2)


  # additional information is carried along as well
  rr <- ruleset(
    rule(mpg > 10, author = "me"),
    rule(cyl %in% c(4, 6, 8), date = "2020-02-29")
  )
  file <- tempfile(fileext = ".yml")
  write_rules(rr, file)

  expect_equal(readLines(file),
               c("- name: 'Rule for: mpg'",
                 "  expr: mpg > 10",
                 "  allow_na: no",
                 "  negate: no",
                 "  author: me",
                 "  index: 1",
                 "- name: 'Rule for: cyl'",
                 "  expr: cyl %in% c(4, 6, 8)",
                 "  allow_na: no",
                 "  negate: no",
                 "  date: '2020-02-29'",
                 "  index: 2"))

  rr2 <- read_rules(file)
  expect_equal(rr, rr2)
})



test_that("Single rule im and export works", {
  rr <- rule(mpg > 10)
  file <- tempfile(fileext = ".yml")
  write_rules(rr, file)

  expect_equal(readLines(file),
               c("- name: 'Rule for: mpg'",
                 "  expr: mpg > 10",
                 "  allow_na: no",
                 "  negate: no",
                 "  index: 1"))

  rr2 <- read_rules(file)
  rr2$index <- NULL
  expect_equal(rr, rr2)
})
