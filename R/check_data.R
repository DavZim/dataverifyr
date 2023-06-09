#' Checks if a dataset confirms to a given set of rules
#'
#' @param x a dataset, either a [`data.frame`], [`dplyr::tibble`], [`data.table::data.table`],
#' [`arrow::arrow_table`], [`arrow::open_dataset`], or [`dplyr::tbl`] (SQL connection)
#' @param rules a list of [`rule`]s
#' @param xname optional, a name for the x variable (only used for errors)
#' @param fail_on_warn if the function should throw an error on a warning
#' @param fail_on_error if the function should throw an error on a failed rule
#'
#' @return a data.frame-like object with one row for each rule and its results
#' @export
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
check_data <- function(x, rules, xname = deparse(substitute(x)),
                       fail_on_warn = FALSE, fail_on_error = FALSE) {

  # if rules is a yaml file, read it in
  if (length(rules) == 1 && is.character(rules)) rules <- read_rules(rules)
  # treat single rule if needed
  if (hasName(rules, "expr")) rules <- ruleset(rules)

  type <- detect_type(class(x))

  # make sure the input dataset has the right class
  if (class(x)[[1]] == "data.frame") {
    if (type == "data.table") {
      x <- data.table::as.data.table(x)
    } else if (type == "dplyr") {
      x <- dplyr::as_tibble(x)
    }
  }

  res <- check_(x, rules, type = type)

  # fails on warning and/or error
  if (fail_on_warn && any(res$warn != "") ||
      fail_on_error && any(res$error != "")) {
    warn <- fail_on_warn && any(res$warn != "")
    err <- fail_on_error && any(res$error != "")

    txt <- sprintf("In dataset '%s' found %s%s%s",
                   xname,
                   if (warn) sprintf("%s warnings", sum(res$warn != "")),
                   if (warn && err) " and ",
                   if (err) sprintf("%s errors", sum(res$error != "")))
    stop(txt)

  }

  res
}

# helper function that detects which type the request should use:
# returns either: base-r, data.table, collectibles (any DBI or arrow backend)
detect_type <- function(cc) {
  if ("data.table" %in% cc) {
    if (!has_pkg("data.table"))
      stop("The data.table package needs to be installed in order to test a data.table OR you can convert the data to a data.frame first!")
    type <- "data.table"

  } else if ("tibble" %in% cc) {
    if (!has_pkg("dplyr"))
      stop("The dplyr package needs to be installed in order to test a tibble OR you can convert the data to a data.frame first!")
    type <- "dplyr"

  } else if (length(cc) == 1 && cc == "data.frame") {

    if (has_pkg("data.table")) {
      type <- "data.table"
    } else if (has_pkg("dplyr")) {
      type <- "dplyr"
    } else {
      type <- "base-r"
    }

  } else if (any(c("tbl_sql", "ArrowObject") %in% cc)) {

    type <- "collectibles"

  } else {
    stop(sprintf(paste("Unknown class of x found: '%s'.",
                       "x must be a data.frame/tibble/data.table or a tbl (SQL table) or ArrowObject."),
                 paste(cc, collapse = ",  ")))
  }
  type
}

# small helper to check if a package is installed
has_pkg <- function(p) requireNamespace(p, quietly = TRUE)


# helper function that collects all warnings
get_warnings <- function(code) {
  out <- c()
  suppressWarnings(withCallingHandlers(code, warning = function(c) out <<- c(out, conditionMessage(c))))
  strip_dplyr_errors(paste(unique(out), collapse = ", "))
}

# helper function that checks the rules given a specific type (~package)
check_ <- function(x, rules, type = c("base-r", "dplyr", "data.table", "collectibles")) {

  type <- match.arg(type)

  # function to create a data.frame
  to_df <- switch(type,
                  "base-r" = data.frame,
                  dplyr = dplyr::tibble,
                  data.table = data.table::data.table,
                  collectibles = dplyr::tibble)
  # function to combine multiple data.frames into a single one by row
  br <- switch(type,
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
        pos <- filter_data_(x, type, e)
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
filter_data_ <- function(x, type, e, return_n = TRUE) {
  if (type == "base-r") {
    # note that the nrow(with(x, x[eval(parse(text = e)), ]))
    # includes NA rows and therefore returns the wrong number of rows
    pos <- with(x, eval(parse(text = e)))
    pos <- if (return_n) sum(pos, na.rm = TRUE) else x[pos, ]
  } else if (type == "dplyr" | type == "collectibles") {
    rr <- dplyr::filter(x, !!str2lang(e))
    if (type == "collectibles") {
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
  } else if (type == "data.table") {
    pos <- x[eval(parse(text = e)), ]
    if (return_n) pos <- nrow(pos)
  }
  pos
}

# strips a dplyr/cli error message of its formatting
# x <- "\033[37;48;5;19m\033[38;5;232mProblem while computing ... .\033[39m\n\033[1mCaused by error in xxx:\033[22m\n\033[33m!\033[39m object 'does_not_exist' not found\033[39;49m"
# x <- "Problem while computing `..1 = eval(parse(text = e))`.\nCaused by error in `does_not_exist %in% c(\"a\", \"b\", \"c\")`:\n! object 'does_not_exist' not found"
# x <- "\033[38;5;232mThere were 2 warnings in `dplyr::filter()`.\nThe first warning was:\033[39m\n\033[38;5;232m\033[36mℹ\033[38;5;232m In argument: `as.numeric(hp) > 0 & as.numeric(hp) < 400`.\033[39m\nCaused by warning:\n\033[33m!\033[39m NAs introduced by coercion\n\033[38;5;232m\033[36mℹ\033[38;5;232m Run \033]8;;ide:run:dplyr::last_dplyr_warnings()\adplyr::last_dplyr_warnings()\033]8;;\a to see the 1 remaining warning.\033[39m"
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
