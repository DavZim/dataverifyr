#' Creates a single data rule
#'
#' @param expr an expression which dictates which determines when a rule is good.
#' Note that the expression is evaluated in `check_data()`, within the given
#' framework. That means, for example if a the data given to `check_data()` is
#' an `arrow` dataset, the expression must be mappable from `arrow` (see also
#' [arrow documentation](https://arrow.apache.org/docs/r/reference/acero.html#function-mappings)).
#' The expression can be given as a string as well.
#' @param name an optional name for the rule for reference
#' @param allow_na does the rule allow for NA values in the data? default value is FALSE.
#' Note that when NAs are introduced in the expression, `allow_na` has no effect.
#' Eg when the rule `as.numeric(vs) %in% c(0, 1)` finds the values of `vs` as
#' `c("1", "A")`, the rule will throw a fail regardless of the value of `allow_na`
#' as the NA is introduced in the expression and is not found in the original data.
#' However, when the values of `vs` are `c("1", NA)`, `allow_na` will have an effect.
#' @param negate is the rule negated, only applies to the expression not allow_na,
#' that is, if `expr = mpg > 10`, `allow_na = TRUE`, and `negate = TRUE`, it would
#' match all `mpg <= 10` as well as NAs.
#' @param ... additional arguments that are carried along for your documentation,
#' but are not used. Could be for example date, person, contact, comment, etc
#'
#' @return The rule values as a list
#' @export
#'
#' @examples
#' r <- rule(mpg > 10)
#' r
#'
#' r2 <- rule(mpg > 10, name = "check that mpg is reasonable", allow_na = TRUE,
#'            negate = FALSE, author = "me", date = Sys.Date())
#' r2
#'
#' check_data(mtcars, r)
#'
#' rs <- ruleset(
#'   rule(mpg > 10),
#'   rule(cyl %in% c(4, 6)), # missing 8
#'   rule(qsec >= 14.5 & qsec <= 22.9)
#' )
#' rs
#' check_data(mtcars, rs)
rule <- function(expr, name = NA, allow_na = FALSE, negate = FALSE, ...) {
  expr <- paste(deparse(substitute(expr)), collapse = "")

  # allows expressions as well as strings
  if (substr(expr, 1, 1) == '"' &&
      substr(expr, nchar(expr), nchar(expr)) == '"')
    expr <- substr(expr, 2, nchar(expr) - 1)

  if (is.na(name))
    name <- paste("Rule for:", paste(get_symbols(expr), collapse = ", "))


  ll <- list(
    name = name,
    expr = expr,
    allow_na = allow_na,
    negate = negate,
    ...
  )
  class(ll) <- "rule"
  ll
}

#' @describeIn rule Prints a rule
#' @param x a rule to print
#' @export
print.rule <- function(x, ...) {
  cat(sprintf("<Verification Rule>\n  expr: '%s'\n  name: '%s'\n  allow NA: %s\n  negated:  %s\n",
              x$expr, x$name, x$allow_na, x$negate))
  nn <- setdiff(names(x), c("expr", "name", "allow_na", "negate"))
  for (n in nn) cat(sprintf("  %s: '%s'\n", n, x[[n]]))
  return(invisible(x))
}

# small helper function to extract the symbols (var names) from an expression
# (given as string)
get_symbols <- function(expr) {
  vv <- getParseData(parse(text = expr, keep.source = TRUE))
  unique(vv[vv$token == "SYMBOL", "text"])
}

#' Creates a set of rules
#'
#' @param ... a list of rules
#'
#' @return the list of rules as a ruleset
#' @export
#'
#' @examples
#' r1 <- rule(mpg > 10)
#' r2 <- rule(mpg < 20)
#' rs <- ruleset(r1, r2)
#' rs
#'
#' rs <- ruleset(
#'   rule(cyl %in% c(4, 6, 8)),
#'   rule(is.numeric(disp))
#' )
#' rs
ruleset <- function(...) {
  ll <- list(...)
  ll <- lapply(seq_along(ll), function(i) {
    l <- ll[[i]]
    if (!"index" %in% names(l)) l$index <- i
    l
  })
  class(ll) <- "ruleset"
  ll
}


#' @describeIn ruleset Prints a ruleset
#' @param x a ruleset to print
#' @param n a maximum number of rules to print
#' @export
print.ruleset <- function(x, n = 3, ...) {
  cat(sprintf("<Verification Ruleset with %s elements>\n", length(x)))

  nn <- min(length(x), n)
  for (r in x[seq(nn)]) {
    cat(sprintf("  [%i] '%s' matching `%s` (allow_na: %s%s)\n",
                r$index, r$name, r$expr, r$allow_na,
                if (r$negate) ", negated" else ""))
  }
  if (nn != length(x))
    cat(sprintf("  ... +%s more. Use print(ruleset, n = 10) to print more.\n",
                length(x) - nn))
  invisible(x)
}

