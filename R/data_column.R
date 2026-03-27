# Internal constructors and validators for structured rule specs (v1).

#' Define a Column Specification for Schema Checks
#'
#' @description
#' Creates a single column declaration used in `ruleset(..., data_columns = ...)`.
#' Column declarations are schema checks (column existence, optionality, and
#' declared type), whereas [`rule()`] is for row-wise value checks.
#'
#' @param col column name.
#' @param type optional declared type (for example `"int"`, `"double"`,
#' `"str"`, `"logical"`). Use `NA_character_` for no type declaration.
#' @param optional logical; if `FALSE`, the column is required.
#' @param description optional free-text description.
#'
#' @return A `data_column` object (list) that can be passed in
#' `ruleset(..., data_columns = list(...))`.
#' @export
#'
#' @examples
#' rs <- ruleset(
#'   rule(price >= 0),
#'   data_columns = list(
#'     data_column("price", type = "double", optional = FALSE),
#'     data_column("note", type = "str", optional = TRUE)
#'   )
#' )
#' rs
#'
#' # combined with row rules and strict schema stopping
#' order_rules <- ruleset(
#'   rule(price >= 0, allow_na = FALSE),
#'   data_columns = list(
#'     data_column("order_id", type = "int", optional = FALSE),
#'     data_column("price", type = "double", optional = FALSE),
#'     data_column("note", type = "str", optional = TRUE)
#'   )
#' )
#'
#' check_data(
#'   data.frame(order_id = 1:3, price = c(10, 20, 30), note = c("ok", NA, "ok")),
#'   order_rules,
#'   stop_on_schema_fail = TRUE
#' )
data_column <- function(col, type = NA_character_, optional = FALSE,
                        description = NA_character_) {
  if (!is.character(col) || length(col) != 1 || is.na(col) || !nzchar(col)) {
    stop("`col` must be a single non-empty character value.")
  }
  if (!is.character(type) || length(type) != 1) {
    stop("`type` must be a single character value (or NA).")
  }
  if (!is.logical(optional) || length(optional) != 1 || is.na(optional)) {
    stop("`optional` must be a single non-missing logical value.")
  }
  if (!is.character(description) || length(description) != 1) {
    stop("`description` must be a single character value (or NA).")
  }

  structure(
    list(
      col = col,
      type = type,
      optional = optional,
      description = description
    ),
    class = "data_column"
  )
}


rule_meta <- function(title = NA_character_, version = NA_character_,
                      description = NA_character_, ...) {
  if (!is.character(title) || length(title) != 1) {
    stop("`title` must be a single character value (or NA).")
  }
  if (!is.character(version) || length(version) != 1) {
    stop("`version` must be a single character value (or NA).")
  }
  if (!is.character(description) || length(description) != 1) {
    stop("`description` must be a single character value (or NA).")
  }

  structure(
    c(
      list(
        title = title,
        version = version,
        description = description
      ),
      list(...)
    ),
    class = "rule_meta"
  )
}


#' Define a Relational Reference Rule
#'
#' @description
#' Creates a rule that checks whether values in a local column exist in a
#' column of a referenced dataset. Use with `check_data()` by supplying `x` as
#' a named list of datasets and setting `data_name` in `ruleset()` (or by
#' ordering the list so the first entry is the primary dataset).
#'
#' @param local_col column name in the primary dataset.
#' @param ref_dataset name of the referenced dataset in the `x` list.
#' @param ref_col column name in the referenced dataset.
#' @param name optional display name for the rule.
#' @param allow_na logical; if `TRUE`, missing values in `local_col` are treated
#' as passing.
#' @param negate logical; if `TRUE`, inverts the rule (values must *not* be in
#' the referenced column).
#' @param ... additional fields attached to the rule object.
#'
#' @return A `reference_rule` object that can be included in `ruleset()`.
#' @export
#'
#' @examples
#' flights <- data.frame(carrier = c("AA", "BB", NA_character_))
#' carriers <- data.frame(carrier_id = c("AA"))
#'
#' rs <- ruleset(
#'   reference_rule(
#'     local_col = "carrier",
#'     ref_dataset = "carriers",
#'     ref_col = "carrier_id",
#'     allow_na = TRUE
#'   ),
#'   data_name = "flights"
#' )
#'
#' check_data(list(flights = flights, carriers = carriers), rs)
#'
#' # negated relation: value must NOT exist in blacklist
#' blacklist <- data.frame(carrier_id = c("XX", "YY"))
#' rs_neg <- ruleset(
#'   reference_rule(
#'     local_col = "carrier",
#'     ref_dataset = "blacklist",
#'     ref_col = "carrier_id",
#'     negate = TRUE,
#'     allow_na = TRUE
#'   ),
#'   data_name = "flights"
#' )
#'
#' check_data(list(flights = flights, blacklist = blacklist), rs_neg)
reference_rule <- function(local_col, ref_dataset, ref_col, name = NA,
                           allow_na = FALSE, negate = FALSE, ...) {
  if (!is.character(local_col) || length(local_col) != 1 || is.na(local_col)) {
    stop("`local_col` must be a single character value.")
  }
  if (!is.character(ref_dataset) || length(ref_dataset) != 1 || is.na(ref_dataset)) {
    stop("`ref_dataset` must be a single character value.")
  }
  if (!is.character(ref_col) || length(ref_col) != 1 || is.na(ref_col)) {
    stop("`ref_col` must be a single character value.")
  }
  if (!is.logical(allow_na) || length(allow_na) != 1 || is.na(allow_na)) {
    stop("`allow_na` must be a single non-missing logical value.")
  }
  if (!is.logical(negate) || length(negate) != 1 || is.na(negate)) {
    stop("`negate` must be a single non-missing logical value.")
  }
  if (is.na(name)) {
    name <- sprintf(
      "Reference rule: %s in %s$%s",
      local_col, ref_dataset, ref_col
    )
  }

  out <- list(
    name = name,
    expr = sprintf("%s %%in%% %s$%s", local_col, ref_dataset, ref_col),
    allow_na = allow_na,
    negate = negate,
    local_col = local_col,
    ref_dataset = ref_dataset,
    ref_col = ref_col,
    ...
  )
  class(out) <- c("reference_rule", "rule")
  out
}


validate_data_columns <- function(data_columns) {
  if (is.null(data_columns)) {
    return(invisible(NULL))
  }
  if (!is.list(data_columns)) {
    stop("`data_columns` must be a list of `data_column()` specs.")
  }

  cols <- vapply(data_columns, function(dc) dc$col, character(1))
  if (anyDuplicated(cols)) {
    stop("`data_columns` contains duplicate column names.")
  }
  invisible(NULL)
}


validate_rules_against_schema <- function(x, rules, extra_columns = "ignore") {
  extra_columns <- match.arg(extra_columns, c("ignore", "warn", "fail"))
  data_columns <- attr(rules, "data_columns", exact = TRUE)
  if (is.null(data_columns) || length(data_columns) == 0) {
    return(empty_schema_results())
  }

  declared <- vapply(data_columns, function(dc) dc$col, character(1))
  present <- names(x)
  schema_rows <- list()

  extra <- setdiff(present, declared)
  if (length(extra)) {
    msg <- sprintf(
      "Found extra columns not declared in `data_columns`: %s",
      paste(extra, collapse = ", ")
    )
    if (extra_columns == "warn") {
      warning(msg)
    } else if (extra_columns == "fail") {
      stop(msg)
    }
  }

  expr_rules <- Filter(function(r) !inherits(r, "reference_rule"), rules)
  used_symbols <- unique(unlist(lapply(expr_rules, function(r) get_symbols(r$expr))))
  unknown <- setdiff(used_symbols, declared)
  if (length(unknown)) {
    stop(sprintf(
      "Unknown symbols found in rules not present in `data_columns`: %s",
      paste(unknown, collapse = ", ")
    ))
  }

  for (dc in data_columns) {
    is_present <- dc$col %in% present
    exists_ok <- is_present || isTRUE(dc$optional)
    schema_rows <- c(
      schema_rows,
      list(schema_result_row(
        name = sprintf("Schema: column '%s' exists", dc$col),
        expr = sprintf("column_exists('%s')", dc$col),
        pass = exists_ok,
        error = if (!exists_ok) sprintf("Required column '%s' is missing.", dc$col) else ""
      ))
    )

    if (is_present && !is.na(dc$type) && nzchar(dc$type)) {
      vv <- x[[dc$col]]
      type_ok <- column_matches_type(vv, dc$type)
      schema_rows <- c(
        schema_rows,
        list(schema_result_row(
          name = sprintf("Schema: column '%s' has type '%s'", dc$col, dc$type),
          expr = sprintf("column_type('%s') == '%s'", dc$col, dc$type),
          pass = type_ok,
          error = if (!type_ok) {
            sprintf("Column '%s' does not match declared type '%s'.", dc$col, dc$type)
          } else {
            ""
          }
        ))
      )
    }
  }

  if (!length(schema_rows)) {
    return(empty_schema_results())
  }

  do.call(rbind, schema_rows)
}


schema_result_row <- function(name, expr, pass, error = "") {
  data.frame(
    check_type = "schema",
    name = name,
    expr = expr,
    allow_na = FALSE,
    negate = FALSE,
    tests = 1L,
    pass = as.integer(isTRUE(pass)),
    fail = as.integer(!isTRUE(pass)),
    warn = "",
    error = error,
    time = as.difftime(0, units = "secs"),
    stringsAsFactors = FALSE
  )
}


empty_schema_results <- function() {
  data.frame(
    check_type = character(),
    name = character(),
    expr = character(),
    allow_na = logical(),
    negate = logical(),
    tests = integer(),
    pass = integer(),
    fail = integer(),
    warn = character(),
    error = character(),
    time = as.difftime(numeric(), units = "secs"),
    stringsAsFactors = FALSE
  )
}


column_matches_type <- function(v, type) {
  type <- tolower(type)
  if (type %in% c("int", "integer")) {
    return(is.integer(v))
  }
  if (type %in% c("dbl", "double", "numeric", "float")) {
    return(is.numeric(v))
  }
  if (type %in% c("str", "string", "chr", "character")) {
    return(is.character(v))
  }
  if (type %in% c("bool", "boolean", "logical")) {
    return(is.logical(v))
  }
  TRUE
}


check_reference_rule <- function(data, r, datasets) {
  if (!r$local_col %in% names(data)) {
    stop(sprintf("Column '%s' used in reference rule was not found.", r$local_col))
  }
  if (!r$ref_dataset %in% names(datasets)) {
    stop(sprintf(
      "In reference rule '%s' referenced dataset '%s' was not supplied.",
      r$name, r$ref_dataset
    ))
  }

  ref_data <- datasets[[r$ref_dataset]]
  if (!r$ref_col %in% names(ref_data)) {
    stop(sprintf(
      "Column '%s' in referenced dataset '%s' was not found.",
      r$ref_col, r$ref_dataset
    ))
  }

  t0 <- Sys.time()
  local_values <- data[[r$local_col]]
  ref_values <- ref_data[[r$ref_col]]
  is_ok <- local_values %in% ref_values
  if (isTRUE(r$allow_na)) {
    is_ok <- is_ok | is.na(local_values)
  }
  if (isTRUE(r$negate)) {
    is_ok <- !is_ok
  }

  nr <- length(local_values)
  pass <- sum(is_ok, na.rm = TRUE)

  data.frame(
    check_type = "reference_rule",
    name = r$name,
    expr = r$expr,
    allow_na = r$allow_na,
    negate = r$negate,
    tests = nr,
    pass = pass,
    fail = nr - pass,
    warn = "",
    error = "",
    time = difftime(Sys.time(), t0, units = "secs"),
    stringsAsFactors = FALSE
  )
}
