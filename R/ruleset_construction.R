#' Add Rules and Rulesets Together
#'
#' @description + allows you to add rules and rulesets into larger rulesets. This
#' can be useful if you want to create a ruleset for a dataset out of checks
#' for other datasets.
#' @param a the first ruleset you wish to add
#' @param b the second ruleset you wish to add
#' @name dataverifyr_plus
datavarifyr_plus <- function(a, b) {
  # This is the method to add *both* rules and rulesets together.
  # Has to be done this way AFAIK to comply with R's
  # [double dispatch](https://yutani.rbind.io/post/double-dispatch-of-s3-method/)
  # semantics.

  if (inherits(a, "rule")  & inherits(b, "rule")) {
    out <- list(a, b)
  } else {
    if (inherits(a, "rule")) a <- list(a)
    if (inherits(b,"rule")) b <- list(b)
    out <- c(a, b)
  }

  out <- out[!duplicated(out)]

  for (i in seq_along(out)) {
    out[[i]]["index"] <- i
  }

  class(out) <- "ruleset"
  return(out)
}

#' @export
#' @rdname dataverifyr_plus
"+.ruleset" <- datavarifyr_plus

#' @export
#' @rdname dataverifyr_plus
"+.rule" <- datavarifyr_plus


###############################################################################

#' Programatically Combine a List of Rules and Rulesets into a Single Ruleset
#'
#' @param rule_ruleset_list a list of rules and rulesets you whish to combine
#' into a single list
#'
#' @return a ruleset which consolidates all the inputs
bind_rules <- function(rule_ruleset_list) {
  Reduce(`+`, rule_ruleset_list)
}
