#' Read and write rules to a yaml file
#'
#' @param x a list of rules
#' @param file a filename
#' @param format output format. `"v1"` writes structured YAML with `meta`,
#' `data-columns`, and `data-rules`. `"pre_v1"` keeps the pre package version 1.0
#' flat-list structure.
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
write_rules <- function(x, file, format = c("v1", "pre_v1")) {
  # if a single rule is supplied: wrap it in a list...
  if (inherits(x, "rule")) x <- ruleset(x)
  if (identical(format, "legacy")) {
    format <- "pre_v1"
  } else {
    format <- match.arg(format)
  }

  ftype <- gsub(".*\\.([^.]+)$", "\\1", file)
  if (ftype %in% c("yml", "yaml")) {
    if (format == "pre_v1") {
      yaml::write_yaml(x, file)
    } else {
      out <- list(
        meta = attr(x, "meta", exact = TRUE),
        "data-columns" = lapply(attr(x, "data_columns", exact = TRUE), unclass),
        "data-rules" = lapply(x, unclass)
      )
      yaml::write_yaml(out, file)
    }
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

  if (is.list(res) && "data-rules" %in% names(res)) {
    if (!"data-rules" %in% names(res)) {
      stop("Malformed v1 yaml: missing `data-rules` section.")
    }

    rr <- lapply(res[["data-rules"]], function(r) {
      r$expr <- paste(r$expr, collapse = "\n")
      if (all(c("local_col", "ref_dataset", "ref_col") %in% names(r))) {
        class(r) <- c("reference_rule", "rule")
      } else {
        class(r) <- "rule"
      }
      r
    })
    class(rr) <- "ruleset"

    if ("meta" %in% names(res)) {
      attr(rr, "meta") <- res[["meta"]]
    }
    if ("data-columns" %in% names(res)) {
      attr(rr, "data_columns") <- lapply(res[["data-columns"]], function(dc) {
        data_column(
          col = dc$col,
          type = if (is.null(dc$type)) NA_character_ else dc$type,
          optional = if (is.null(dc$optional)) FALSE else dc$optional,
          description = if (is.null(dc$description)) NA_character_ else dc$description
        )
      })
    }
    return(rr)
  }

  # detect malformed structured format (has sections but not data-rules)
  if (is.list(res) && any(c("meta", "data-columns", "data-rules") %in% names(res)) &&
      !"data-rules" %in% names(res)) {
    stop("Malformed v1 yaml: missing `data-rules` section.")
  }

  rr <- lapply(res, function(r) {
    r$expr <- paste(r$expr, collapse = "\n")
    class(r) <- "rule"
    r
  })
  class(rr) <- "ruleset"

  # if a single rule was supplied: unpack it again
  if (length(rr) == 1) rr <- rr[[1]]
  rr
}
