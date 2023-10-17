test_that("Rules can be added together", {
      rule_1 <- rule(mpg > 10)
      rule_2 <- rule(hp > 10)
      expect_equal(
        rule_1 + rule_2,
        ruleset(rule_1, rule_2)
      )
})

test_that("Rules can be added to rulesets", {
      rule_1 <- rule(mpg > 10)
      rule_2 <- rule(hp > 10)
      rule_3 <- rule(name == "henry")

      expect_equal(
        rule_1 + ruleset(rule_2, rule_3),
        ruleset(rule_1, rule_2, rule_3)
      )
})

test_that("rulesets can be added to rules", {
      rule_1 <- rule(mpg > 10)
      rule_2 <- rule(hp > 10)
      rule_3 <- rule(name == "henry")

      expect_equal(
        ruleset(rule_1, rule_2) + rule_3,
        ruleset(rule_1, rule_2, rule_3)
      )
})

test_that("rulesets can be added to rules", {
      rule_1 <- rule(mpg > 10)
      rule_2 <- rule(hp > 10)
      rule_3 <- rule(name == "henry")
      rule_4 <- rule(sex == "female")

      expect_equal(
        ruleset(rule_1, rule_2) + ruleset(rule_3, rule_4),
        ruleset(rule_1, rule_2, rule_3, rule_4)
      )
})

test_that("duplicates are removed when adding rules and rulesets", {
      rule_1 <- rule(mpg > 10)
      rule_2 <- rule(hp > 10)
      expect_equal(
        length(ruleset(rule_1, rule_2) + ruleset(rule_1, rule_2)),
        2
      )

      expect_equal(
        length(rule_1 + rule_2 + ruleset(rule_1, rule_2) + ruleset(rule_1, rule_2)),
        2
      )
})

test_that("bind_rules works", {
      rule_1 <- rule(mpg > 10)
      rule_2 <- rule(hp > 10)
      rule_3 <- rule(name == "henry")
      rule_4 <- rule(sex == "female")

      expect_equal(
        bind_rules(list(rule_1, rule_2, ruleset(rule_3, rule_4))),
        ruleset(rule_1, rule_2, rule_3, rule_4)
      )

      expect_equal(
        bind_rules(list(ruleset(rule_1, rule_2), ruleset(rule_3, rule_4))),
        ruleset(rule_1, rule_2, rule_3, rule_4)
      )

      expect_equal(
        bind_rules(list(rule_1, ruleset(rule_2), ruleset(rule_3, rule_4))),
        ruleset(rule_1, rule_2, rule_3, rule_4)
      )
})
