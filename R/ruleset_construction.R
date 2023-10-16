#' Add Rulesets Together
#'
#' `+` allows you to concatenate several rulesets into a larger rulesets. This
#' can be useful if you want to create a ruleset for a dataset out of checks
#' for other datasets.
#'
#' @param a the first ruleset you wish to add
#' @param a the second ruleset you wish to add
#' @rdname ruleset_add
#' @export
"+.ruleset" <- function(a,b) {
	out <- c(a,b)
	class(out) <- "ruleset"

    for (i in seq_along(out)){
        out[[i]]['index'] <- i
    }

	out
}

#' @rdname ruleset_add
#' @export
"%+%" <- `+.ruleset`
