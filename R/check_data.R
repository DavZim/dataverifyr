#' Checks if a dataset confirms to a given set of rules
#'
#' @param x a dataset, either a [`data.frame`], [`dplyr::tibble`], [`data.table::data.table`],
#' [`arrow::arrow_table`], [`arrow::open_dataset`], or [`dplyr::tbl`] (SQL connection).
#' Can also be a named list of datasets when using reference rules.
#' @param rules a list of [`rule`]s
#' @param xname optional, a name for the x variable (only used for errors)
#' @param stop_on_fail when any of the rules fail, throw an error with stop
#' @param stop_on_warn when a warning is found in the code execution, throw an error with stop
#' @param stop_on_error when an error is found in the code execution, throw an error with stop
#' @param stop_on_schema_fail when any schema checks fail, throw an error with stop
#' @param extra_columns how to treat columns in `x` that are not declared in
#' optional `data_columns` attached to a ruleset. One of `"ignore"` (default),
#' `"warn"`, or `"fail"`.
#'
#' @return a data.frame-like object with one row for each rule and its results
#' @export
#'
#' @seealso [detect_backend()]
#'
#' @examples
#' rs <- ruleset(
#'   rule(mpg > 10),
#'   rule(cyl %in% c(4, 6)), # missing 8
#'   rule(qsec >= 14.5 & qsec <= 22.9)
#' )
#' rs
#'
#' check_data(mtcars, rs)
#'
#' # schema + relation checks in one output
#' orders <- data.frame(order_id = 1:3, customer_id = c(10, 99, NA), amount = c(10, -5, 20))
#' customers <- data.frame(customer_id = c(10, 11))
#'
#' rs2 <- ruleset(
#'   rule(amount >= 0, name = "amount non-negative"),
#'   reference_rule(
#'     local_col = "customer_id",
#'     ref_dataset = "customers",
#'     ref_col = "customer_id",
#'     allow_na = TRUE
#'   ),
#'   data_columns = list(
#'     data_column("order_id", type = "int", optional = FALSE),
#'     data_column("customer_id", type = "double", optional = FALSE),
#'     data_column("amount", type = "double", optional = FALSE)
#'   ),
#'   data_name = "orders"
#' )
#'
#' check_data(list(orders = orders, customers = customers), rs2)
check_data <- function(x, rules, xname = deparse(substitute(x)),
                       stop_on_fail = FALSE, stop_on_warn = FALSE,
                       stop_on_error = FALSE, stop_on_schema_fail = FALSE,
                       extra_columns = c("ignore", "warn", "fail")) {

  extra_columns <- match.arg(extra_columns)
  # if rules is a yaml file, read it in
  if (length(rules) == 1 && is.character(rules)) rules <- read_rules(rules)
  # treat single rule if needed
  if (hasName(rules, "expr")) rules <- ruleset(rules)

  datasets <- NULL
  if (is.list(x) && !inherits(x, "data.frame")) {
    datasets <- x
    data_name <- attr(rules, "data_name", exact = TRUE)
    if (is.null(data_name)) {
      data_name <- names(datasets)[[1]]
    }
    if (is.null(data_name) || is.na(data_name) || !nzchar(data_name)) {
      stop(paste(
        "When `x` is a list, datasets must be named or",
        "`ruleset(..., data_name=...)` must be set."
      ))
    }
    if (!data_name %in% names(datasets)) {
      stop(sprintf("The primary dataset '%s' was not found in `x`.", data_name))
    }
    x <- datasets[[data_name]]
  }

  schema_res <- validate_rules_against_schema(
    x, rules, extra_columns = extra_columns
  )

  backend <- detect_backend(x)

  # make sure the input dataset has the right class
  if (class(x)[[1]] == "data.frame") {
    if (backend == "data.table") {
      x <- data.table::as.data.table(x)
    } else if (backend == "dplyr") {
      x <- dplyr::as_tibble(x)
    }
  }

  expr_rules <- Filter(function(r) !inherits(r, "reference_rule"), rules)
  ref_rules <- Filter(function(r) inherits(r, "reference_rule"), rules)

  if (length(expr_rules)) {
    res <- check_(x, expr_rules, backend = backend)
  } else {
    res <- data.frame(
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

  if (length(ref_rules)) {
    if (is.null(datasets)) {
      stop("Reference rules require `x` to be a named list of datasets.")
    }
    ref_res <- do.call(rbind, lapply(ref_rules, function(r) {
      check_reference_rule(x, r, datasets)
    }))

    res <- switch(
      backend,
      "base-r" = rbind(res, ref_res),
      dplyr = dplyr::bind_rows(res, ref_res),
      data.table = data.table::rbindlist(list(res, ref_res), fill = TRUE),
      collectibles = dplyr::bind_rows(res, ref_res)
    )
  }

  if (nrow(schema_res)) {
    res <- switch(
      backend,
      "base-r" = rbind(schema_res, res),
      dplyr = dplyr::bind_rows(schema_res, res),
      data.table = data.table::rbindlist(list(schema_res, res), fill = TRUE),
      collectibles = dplyr::bind_rows(schema_res, res)
    )
  }

  # stops on fail, warning and/or error
  is_schema <- if ("check_type" %in% names(res)) {
    res$check_type == "schema"
  } else {
    rep(FALSE, nrow(res))
  }
  is_rule <- !is_schema

  fail <- stop_on_fail && any(res$fail[is_rule] != 0)
  schema_fail <- stop_on_schema_fail && any(res$fail[is_schema] != 0)
  warn <- stop_on_warn && any(res$warn != "")
  err <- stop_on_error && any(res$error != "")

  if (fail || schema_fail || warn || err) {
    tt <- paste(c(
      if (fail) sprintf("%s rule fails", sum(res$fail[is_rule] != 0)),
      if (schema_fail) sprintf("%s schema fails", sum(res$fail[is_schema] != 0)),
      if (warn) sprintf("%s warnings", sum(res$warn != "")),
      if (err) sprintf("%s errors", sum(res$error != ""))
    ), collapse = ", ")

    stop(sprintf("In dataset '%s' found %s", xname, tt))
  }

  res
}

#' Detects the backend which will be used for checking the rules
#'
#' @description
#' The detection will be made based on the class of the object as well as the packages installed.
#' For example, if a `data.frame` is used, it will look if `data.table` or `dplyr` are installed
#' on the system, as they provide more speed.
#' Note the main functions will revert the
#'
#' @param x The data object, ie a data.frame, tibble, data.table, arrow, or DBI object
#'
#' @return a single character element with the name of the backend to use.
#' One of `base-r`, `data.table`, `dplyr`, `collectibles` (for arrow or DBI objects)
#' @export
#'
#' @seealso [check_data()]
#' @examples
#' data <- mtcars
#' detect_backend(data)
detect_backend <- function(x) {
  cc <- class(x)
  if ("data.table" %in% cc) {
    if (!has_pkg("data.table"))
      stop(paste(
        "The data.table package needs to be installed in order to test a data.table",
        "OR you can convert the data to a data.frame first!"
      ))
    backend <- "data.table"

  } else if (any(c("tibble", "tbl_df") %in% cc)) {
    if (!has_pkg("dplyr"))
      stop(paste(
        "The dplyr package needs to be installed in order to test a tibble OR",
        "you can convert the data to a data.frame first!"
      ))
    backend <- "dplyr"

  } else if (length(cc) == 1 && cc == "data.frame") {

    if (has_pkg("data.table")) {
      backend <- "data.table"
    } else if (has_pkg("dplyr")) {
      backend <- "dplyr"
    } else {
      backend <- "base-r"
    }

  } else if ("tbl_sql" %in% cc) {

    if (!has_pkg("DBI") || !has_pkg("dbplyr"))
      stop("The DBI and dbplyr packages need to be installed in order to test a tbl_sql.")

    backend <- "collectibles"

  } else if ("ArrowObject" %in% cc) {

    if (!has_pkg("arrow") || !has_pkg("dbplyr"))
      stop("The arrow and dbplyr packages need to be installed in order to test an ArrowObject.")

    backend <- "collectibles"

  } else {

    stop(sprintf(paste(
      "Unknown class of x found: '%s'.",
      "x must be a data.frame/tibble/data.table or a tbl (SQL table) or ArrowObject."
    ), paste(cc, collapse = ",  ")))
  }
  backend
}

# small helper to check if a package is installed
has_pkg <- function(p) requireNamespace(p, quietly = TRUE)


# helper function that collects all warnings
get_warnings <- function(code) {
  out <- c()
  suppressWarnings(withCallingHandlers(code,
                                       warning = function(c) out <<- c(out, conditionMessage(c))))
  strip_dplyr_errors(paste(unique(out), collapse = ", "))
}

# helper function that checks the rules given a specific backend (~package)
check_ <- function(x, rules, backend = c("base-r", "dplyr", "data.table", "collectibles")) {

  backend <- match.arg(backend)

  # function to create a data.frame
  to_df <- switch(backend,
                  "base-r" = data.frame,
                  dplyr = dplyr::tibble,
                  data.table = data.table::data.table,
                  collectibles = dplyr::tibble)
  # function to combine multiple data.frames into a single one by row
  br <- switch(backend,
               "base-r" = function(l) do.call(rbind, l),
               dplyr = dplyr::bind_rows,
               data.table = data.table::rbindlist,
               collectibles = dplyr::bind_rows)

  ll <- lapply(rules, function(r) {

    e <- r$expr

    # negate expression if needed
    if ("negate" %in% names(r) && r$negate) {
      e <- paste0("!(", e, ")")
    }

    # add the is.na guards when allow_na = TRUE
    if ("allow_na" %in% names(r) && r$allow_na) {
      v <- get_symbols(e)
      e <- paste("(", e, ")", "|", paste("is.na(", v, ")",
                                         sep = "", collapse = " | "))
    }


    # evaluate the expression and collect warnings and errors...
    t0 <- Sys.time()
    nr <- if ("tbl_sql" %in% class(x)) {
      dplyr::pull(dplyr::collect(dplyr::summarise(x, n = dplyr::n())), n)
    } else {
      nrow(x)
    }
    warns <- ""
    err <- ""
    oc <- Sys.getenv("NO_COLOR") # to turn off colors in errors/warnings
    Sys.setenv(NO_COLOR = "OFF")
    pos <- tryCatch({
      warns <- get_warnings({
        pos <- filter_data_(x, backend, e)
      })
      pos
    }, error = function(err) {
      strip_dplyr_errors(conditionMessage(err))
    })

    Sys.setenv("NO_COLOR" = oc) # to turn on colors in errors/warnings

    if (is.character(pos)) {
      err <- pos
      pos <- 0
    }
    tt <- difftime(Sys.time(), t0, units = "secs")
    to_df(
      check_type = "row_rule",
      name = r$name,
      expr = r$expr,
      allow_na = r$allow_na,
      negate = r$negate,
      tests = nr,
      pass = pos,
      fail = nr - pos,
      warn = warns,
      error = err,
      time = tt
    )
  })

  br(ll)
}

# internal helper function that filters a dataset x
# when return_n = FALSE the data is returned, otherwise the number of rows
filter_data_ <- function(x, backend, e, return_n = TRUE) {
  if (backend == "base-r") {
    # note that the nrow(with(x, x[eval(parse(text = e)), ]))
    # includes NA rows and therefore returns the wrong number of rows
    pos <- with(x, eval(parse(text = e)))
    pos <- if (return_n) sum(pos, na.rm = TRUE) else x[pos, ]
  } else if (backend == "dplyr" || backend == "collectibles") {
    rr <- dplyr::filter(x, !!str2lang(e))
    if (backend == "collectibles") {
      if (return_n) {
        pos <- dplyr::pull(
          dplyr::collect(dplyr::summarise(rr, n = dplyr::n())),
          n
        )
      } else {
        pos <- dplyr::collect(rr)
      }
    } else { # dplyr
      pos <- if (return_n) nrow(rr) else rr
    }
  } else if (backend == "data.table") {
    pos <- x[eval(parse(text = e)), ]
    if (return_n) pos <- nrow(pos)
  }
  pos
}

# strips a dplyr/cli error message of its formatting
# nolint start
# x <- "\033[37;48;5;19m\033[38;5;232mProblem while computing ... .\033[39m\n\033[1mCaused by error in xxx:\033[22m\n\033[33m!\033[39m object 'does_not_exist' not found\033[39;49m"
# x <- "Problem while computing `..1 = eval(parse(text = e))`.\nCaused by error in `does_not_exist %in% c(\"a\", \"b\", \"c\")`:\n! object 'does_not_exist' not found"
# x <- "\033[38;5;232mThere were 2 warnings in `dplyr::filter()`.\nThe first warning was:\033[39m\n\033[38;5;232m\033[36mℹ\033[38;5;232m In argument: `as.numeric(hp) > 0 & as.numeric(hp) < 400`.\033[39m\nCaused by warning:\n\033[33m!\033[39m NAs introduced by coercion\n\033[38;5;232m\033[36mℹ\033[38;5;232m Run \033]8;;ide:run:dplyr::last_dplyr_warnings()\adplyr::last_dplyr_warnings()\033]8;;\a to see the 1 remaining warning.\033[39m"
# nolint end
strip_dplyr_errors <- function(x) {
  if (substr(x, 1, 1) == "\033") {
    r <- gsub("\033.*\033\\[33m\\!\033\\[39m ", "", x)
    r <- gsub("\033.*$", "", r)
  } else {
    # testthat automatically turns off the colorful errors...
    r <- gsub(".*\\n\\! ", "", x)
    r <- gsub("i Run.*$", "", r) # remove multiline warnings
  }
  gsub("\\n$", "", r)
}
