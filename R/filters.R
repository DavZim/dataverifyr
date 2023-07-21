#' Filters a result dataset for the values that failed the verification
#'
#' @param res a result data.frame as outputted from [`check_data()`] or a ruleset
#' @param x a dataset that was used in [`check_data()`]
#' @param per_rule if set to TRUE, a list of filtered data is returned, one for
#' each failed verification rule. If set to FALSE, a data.frame is returned of
#' the values that fail any rule.
#'
#' @return the dataset with the entries that did not match the given rules
#' @export
#'
#' @examples
#' rules <- ruleset(
#'   rule(mpg > 10 & mpg < 30), # mpg goes up to 34
#'   rule(cyl %in% c(4, 8)), # missing 6 cyl
#'   rule(vs %in% c(0, 1), allow_na = TRUE)
#' )
#'
#' res <- check_data(mtcars, rules)
#'
#' filter_fails(res, mtcars)
#' filter_fails(res, mtcars, per_rule = TRUE)
#'
#' # alternatively, the first argument can also be a ruleset
#' filter_fails(rules, mtcars)
#' filter_fails(rules, mtcars, per_rule = TRUE)
filter_fails <- function(res, x, per_rule = FALSE) {

  if (class(res)[[1]] == "ruleset") {

    eorig <- sapply(res, function(x) x$expr)
    negated <- sapply(res, function(x) x$negate)
    allow_na <- sapply(res, function(x) x$allow_na)

  } else { # result of check_data

    stopifnot(is.data.frame(res))
    stopifnot(all(
      c("name", "expr", "allow_na", "negate", "pass", "fail", "tests") %in% names(res)
    ))

    fails <- res$fail != 0
    eorig <- res$expr[fails]
    negated <- res$negate[fails]
    allow_na <- res$allow_na[fails]

    if (all(!fails)) return(x[0, ])

  }

  type <- detect_type(class(x))

  # add negated values
  e <- ifelse(negated, paste0("!(", eorig, ")"), eorig)

  # add is.na guards
  e <- sapply(seq_along(e), function(i) {
    ifelse(
      allow_na[i],
      paste("(", e[i], ")", "|", paste("is.na(", get_symbols(e[i]), ")",
                                       sep = "", collapse = " | ")),
      # due to negation, add anti-is.na guard
      paste("(", e[i], ")", "&", paste("!is.na(", get_symbols(e[i]), ")",
                                       sep = "", collapse = " & "))
    )
  })

  # negate everything because we care for fails not for matches!
  e <- paste0("!(", e, ")")

  # make sure the input dataset has the right class
  if (class(x)[[1]] == "data.frame") {
    if (type == "data.table" && requireNamespace("data.table", quietly = TRUE)) {
      x <- data.table::as.data.table(x)
    } else if (type == "dplyr" && requireNamespace("dplyr", quietly = TRUE)) {
      x <- dplyr::as_tibble(x)
    }
  }

  ll <- lapply(e, filter_data_, x = x, type = type, return_n = FALSE)
  if (per_rule) {
    ll <- setNames(ll, eorig)[sapply(ll, nrow) != 0]
  } else {
    ll <- do.call(rbind, ll)
  }
  ll
}
