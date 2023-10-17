#' Add Rules and Rulesets Together
#'
#' `+` allows you to add rules and rulesets into larger rulesets. This
#' can be useful if you want to create a ruleset for a dataset out of checks
#' for other datasets.
#'
#' @param a the first ruleset you wish to add
#' @param a the second ruleset you wish to add
#' @rdname ruleset_add
datavarifyr_plus  <- function(a,b) {
    # This is the method to add *both* rules and rulesets together.
    # Has to be done this way AFAIK to comply with R's
    # [double dispatch](https://yutani.rbind.io/post/double-dispatch-of-s3-method/)
    # semantics.

    if (class(a) == "ruleset" & class(b)=="ruleset") {
        out <- c(a,b)
    } else if (class(a) == "ruleset" & class(b) == "rule"){
        out <- c(a,list(b))
    } else if (class(a) == "rule" & class(b) == "ruleset"){
        out <- c(list(a),b)
    } else{
        out <- list(a,b)
    }

    for (i in seq_along(out)){
        out[[i]]['index'] <- i
    }

    class(out) <- "ruleset"
    return(out)
}

#' @export
#' @rdname dataverifyr_add
"+.ruleset" <- datavarifyr_plus

#' @export
#' @rdname dataverifyr_add
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

