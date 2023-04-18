




#' Filters a result dataset for the values that failed the verification
#'
#' @param res a result data.frame as outputted from [`check_data()`]
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
#' rule(mpg > 10 & mpg < 30), # mpg goes up to 34
#'   rule(cyl %in% c(4, 8)), # missing 6 cyl
#'   rule(vs %in% c(0, 1), allow_na = TRUE)
#' )
#'
#' res <- check_data(mtcars, rules)
#'
#' err <- filter_errors(res, mtcars)
#' err
filter_fails <- function(res, x, per_rule = FALSE) {
  stopifnot(is.data.frame(res))
  stopifnot(all(
    c("name", "expr", "allow_na", "negate", "pass", "fail", "tests") %in% names(res)
  ))

  fails <- res$fail != 0
  e <- res$expr[fails]
  type <- detect_type(class(x))

  # add negated values
  e <- ifelse(res$negate[fails], paste0("!(", e, ")"), e)

  # add is.na guards
  e <- sapply(seq_along(e), function(i) {
    ifelse(
      res$allow_na[fails][i],
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
    if (type == "data.table") {
      x <- data.table::as.data.table(x)
    } else if (type == "dplyr") {
      x <- dplyr::as_tibble(x)
    }
  }

  ll <- lapply(e, filter_data_, x = x, type = type, return_n = FALSE)
  if (per_rule) ll <- setNames(ll, res$expr[fails]) else ll <- do.call(rbind, ll)
  ll
}
