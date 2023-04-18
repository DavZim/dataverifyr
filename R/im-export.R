#' Read and write rules to a yaml file
#'
#' @param x a list of rules
#' @param file a filename
#'
#' @return the filename invisibly
#' @export
#'
#' @examples
#' rr <- ruleset(
#'   rule(mpg > 10),
#'   rule(cyl %in% c(4, 6, 8))
#' )
#' file <- tempfile(fileext = ".yml")
#' write_rules(rr, file)
write_rules <- function(x, file) {
  # if a single rule is supplied: wrap it in a list...
  if (class(x) == "rule") x <- ruleset(x)

  ftype <- gsub(".*\\.([^.]+)$", "\\1", file)
  if (ftype %in% c("yml", "yaml")) {
    yaml::write_yaml(x, file)
  } else {
    stop("At the moment only .yaml files are supported")
  }
}


#' @describeIn write_rules reads a ruleset back in
#' @export
read_rules <- function(file) {
  ftype <- gsub(".*\\.([^.]+)$", "\\1", file)
  if (ftype %in% c("yml", "yaml")) {
    res <- yaml::read_yaml(file)
  } else {
    stop("At the moment only .yaml files are supported")
  }

  res <- lapply(res, function(r) {
    class(r) <- "rule"
    r
  })
  class(res) <- "ruleset"

  # if a single rule was supplied: unpack it again
  if (length(res) == 1) res <- res[[1]]
  res
}
