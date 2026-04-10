#' Describes a dataset
#'
#' Note that the current version is in the beta stadium at best, that means the
#' R-native formats (data.frame, dplyr/tibble, or data.table) are a lot faster
#' than arrow or SQL-based datasets.
#'
#' @param x a dataset, either a [`data.frame`], [`dplyr::tibble`], [`data.table::data.table`],
#' [`arrow::arrow_table`], [`arrow::open_dataset`], or [`dplyr::tbl`] (SQL connection)
#' @param skip_ones logical, whether values that occur exactly once should be omitted
#' from `most_frequent`
#' @param digits integer, number of digits to round numeric values in `most_frequent`
#' @param top_n integer, number of most frequent values to include in `most_frequent`;
#' set to `0` to skip the `most_frequent` computation
#' @param fast logical, when `TRUE` skip expensive fields (`n_distinct`, `median`)
#' by returning `NA` for them
#'
#' @return a `data.frame`, `dplyr::tibble`, or `data.table::data.table` containing
#' a summary of the dataset given
#' @details
#' Numeric values in `most_frequent` are rounded to `digits` (default: 4).
#' If a variable has at most 1 distinct value, `most_frequent` is left empty.
#' By default, values with count 1 are omitted from `most_frequent`.
#' @export
#'
#' @seealso Similar to
#' [skimr::skim()]( https://CRAN.R-project.org/package=skimr),
#' [summarytools::dfSummary()]( https://CRAN.R-project.org/package=summarytools),
#' and [gtExtras::gt_plt_summary()]( https://CRAN.R-project.org/package=gtExtras)
#'
#' @examples
#' describe(mtcars)
describe <- function(x, skip_ones = TRUE, digits = 4, top_n = 3, fast = FALSE) {

  if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n) || top_n < 0) {
    stop("`top_n` must be a single number >= 0.")
  }
  top_n <- as.integer(top_n)
  if (!is.logical(fast) || length(fast) != 1 || is.na(fast)) {
    stop("`fast` must be a single non-missing logical value.")
  }

  backend <- detect_backend(x)

  # make sure the input dataset has the right class
  if (class(x)[[1]] == "data.frame") {
    if (backend == "data.table") {
      x <- data.table::as.data.table(x)
    } else if (backend == "dplyr") {
      x <- dplyr::as_tibble(x)
    }
  }

  if (backend == "base-r") {
    describe_base_r(x, max_n = top_n, skip_ones = skip_ones, digits = digits, fast = fast)
  } else if (backend == "dplyr") {
    describe_dplyr(x, max_n = top_n, skip_ones = skip_ones, digits = digits, fast = fast)
  } else if (backend == "data.table") {
    describe_data.table(x, max_n = top_n, skip_ones = skip_ones, digits = digits, fast = fast)
  } else if (backend == "collectibles") {
    describe_collectibles(x, max_n = top_n, skip_ones = skip_ones, digits = digits, fast = fast)
  } else {
    stop(sprintf("Could not detect backend to describe %s", paste(class(x), collapse = ", ")))
  }
}

# internal function to see which values should use the min/max etc part
is_numeric <- function(v) {
  any(class(v) %in% c("integer", "numeric", "POSIXt"))
}

is_roundable_numeric <- function(v) {
  any(class(v) %in% c("integer", "numeric"))
}

format_most_frequent <- function(values, counts, skip_ones = TRUE, digits = 4) {
  if (length(unique(values)) <= 1) return("")
  if (skip_ones) {
    keep <- counts > 1
    values <- values[keep]
    counts <- counts[keep]
  }
  if (length(values) == 0) return("")
  if (is_roundable_numeric(values)) values <- round(values, digits = digits)
  paste(sprintf("%s (%s)", values, counts), collapse = ", ")
}

top_counts <- function(v, max_n = 3) {
  uv <- unique(v)
  if (max_n <= 0) return(list(values = uv[0], counts = integer(0)))
  tab <- tabulate(match(v, uv))
  od <- order(tab, decreasing = TRUE)[seq(min(max_n, length(tab)))]
  list(values = uv[od], counts = tab[od])
}

# eg x <- mtcars
describe_base_r <- function(x, max_n = 3, skip_ones = TRUE, digits = 4, fast = FALSE) {
  ll <- lapply(
    seq_len(ncol(x)),
    function(i) {
      v <- x[[i]]
      type <- class(v)[[1]]
      is_num <- is_numeric(v)

      tc <- top_counts(v, max_n = max_n)

      nz <- if (!is_num) nchar(as.character(v))

      data.frame(
        var = names(x)[[i]],
        type = type,
        n = length(v),
        n_distinct = if (fast) NA_integer_ else length(unique(v)),
        n_na = sum(is.na(v)),
        most_frequent = if (fast) {
          NA_character_
        } else {
          format_most_frequent(tc$values, tc$counts, skip_ones = skip_ones, digits = digits)
        },

        min = as.numeric(min(if (is_num) v else nz, na.rm = TRUE)),
        mean = as.numeric(mean(if (is_num) v else nz, na.rm = TRUE)),
        median = if (fast) {
          NA_real_
        } else {
          as.numeric(stats::median(if (is_num) v else nz, na.rm = TRUE))
        },
        max = as.numeric(max(if (is_num) v else nz, na.rm = TRUE)),
        sd = as.numeric(stats::sd(if (is_num) v else nz, na.rm = TRUE))
      )
    }
  )

  do.call(rbind, ll)
}

# eg x <- tibble::as_tibble(mtcars)
describe_dplyr <- function(x, max_n = 3, skip_ones = TRUE, digits = 4, fast = FALSE) {
  ll <- lapply(
    names(x),
    function(v) {
      vv <- x[[v]]
      tc <- top_counts(vv, max_n = max_n)

      type <- class(vv)[[1]]
      is_num <- is_numeric(vv)
      mf <- if (fast) {
        NA_character_
      } else {
        format_most_frequent(tc$values, tc$counts, skip_ones = skip_ones, digits = digits)
      }

      nz <- if (!is_num) nchar(as.character(x[[v]]))
      if (fast) {
        dplyr::summarise(
          x,
          var = v,
          type = type,
          n = dplyr::n(),
          n_distinct = as.integer(NA),
          n_na = sum(is.na(.data[[v]])),
          most_frequent = mf,
          min = as.numeric(min(if (is_num) .data[[v]] else nz, na.rm = TRUE)),
          mean = as.numeric(mean(if (is_num) .data[[v]] else nz, na.rm = TRUE)),
          median = as.numeric(NA),
          max = as.numeric(max(if (is_num) .data[[v]] else nz, na.rm = TRUE)),
          sd = as.numeric(stats::sd(if (is_num) .data[[v]] else nz, na.rm = TRUE))
        )
      } else {
        dplyr::summarise(
          x,
          var = v,
          type = type,
          n = dplyr::n(),
          n_distinct = dplyr::n_distinct(.data[[v]]),
          n_na = sum(is.na(.data[[v]])),
          most_frequent = mf,
          min = as.numeric(min(if (is_num) .data[[v]] else nz, na.rm = TRUE)),
          mean = as.numeric(mean(if (is_num) .data[[v]] else nz, na.rm = TRUE)),
          median = as.numeric(stats::median(if (is_num) .data[[v]] else nz, na.rm = TRUE)),
          max = as.numeric(max(if (is_num) .data[[v]] else nz, na.rm = TRUE)),
          sd = as.numeric(stats::sd(if (is_num) .data[[v]] else nz, na.rm = TRUE))
        )
      }
    }
  )

  dplyr::bind_rows(ll)
}

# eg x <- data.table::as.data.table(mtcars)
describe_data.table <- function(x, max_n = 3, skip_ones = TRUE, digits = 4, fast = FALSE) { # nolint
  ll <- lapply(
    names(x),
    function(v) {
      vv <- x[[v]]
      tc <- top_counts(vv, max_n = max_n)

      type <- class(vv)[[1]]
      is_num <- is_numeric(vv)
      mf <- if (fast) {
        NA_character_
      } else {
        format_most_frequent(tc$values, tc$counts, skip_ones = skip_ones, digits = digits)
      }

      nz <- if (!is_num) nchar(as.character(x[[v]]))

      x[, .(
        var = v,
        type = type,
        n = .N,
        n_distinct = if (fast) NA_integer_ else data.table::uniqueN(get(v)),
        n_na = sum(is.na(get(v))),
        most_frequent = mf,

        min = as.numeric(min(if (is_num) get(v) else nz, na.rm = TRUE)),
        mean = as.numeric(mean(if (is_num) get(v) else nz, na.rm = TRUE)),
        median = if (fast) {
          NA_real_
        } else {
          as.numeric(stats::median(if (is_num) get(v) else nz, na.rm = TRUE))
        },
        max = as.numeric(max(if (is_num) get(v) else nz, na.rm = TRUE)),
        sd = as.numeric(stats::sd(if (is_num) get(v) else nz, na.rm = TRUE))
      )]
    }
  )

  data.table::rbindlist(ll)
}


# RSQLite, duckdb, arrow etc
describe_collectibles <- function(x, max_n = 3, skip_ones = TRUE, digits = 4, fast = FALSE) {
  colmeta <- describe_collectibles_colmeta(x)
  vars <- colmeta$var

  stats_by_var <- describe_collectibles_stats_with_fallback(x, colmeta, fast = fast)

  mf_by_var <- if (fast) {
    stats::setNames(rep(NA_character_, length(vars)), vars)
  } else if (max_n == 0L) {
    stats::setNames(rep("", length(vars)), vars)
  } else {
    describe_collectibles_most_frequent(
      x,
      vars = vars,
      max_n = max_n,
      skip_ones = skip_ones,
      digits = digits
    )
  }

  ll <- lapply(seq_len(nrow(colmeta)), function(i) {
    v <- colmeta$var[[i]]
    stats <- stats_by_var[[v]]
    dplyr::tibble(
      var = v,
      type = colmeta$type[[i]],
      n = stats$n,
      n_distinct = stats$n_distinct,
      n_na = stats$n_na,
      most_frequent = mf_by_var[[v]],
      min = stats$min,
      mean = stats$mean,
      median = stats$median,
      max = stats$max,
      sd = stats$sd
    )
  })

  dplyr::bind_rows(ll)
}


describe_collectibles_stats <- function(x, v, is_num, fast = FALSE) {
  out <- try(
    describe_collectibles_stats_single_query(x, v, is_num = is_num, fast = fast),
    silent = TRUE
  )
  if (!inherits(out, "try-error")) return(out)

  vals <- dplyr::pull(
    dplyr::collect(
      dplyr::select(x, dplyr::all_of(v))
    ),
    1
  )
  describe_vector_stats(vals, is_num = is_num, fast = fast)
}


describe_collectibles_stats_with_fallback <- function(x, colmeta, fast = FALSE) {
  out <- try(
    describe_collectibles_stats_batched(x, colmeta = colmeta, fast = fast),
    silent = TRUE
  )
  if (!inherits(out, "try-error")) return(out)

  out <- list()
  failed <- character()
  for (i in seq_len(nrow(colmeta))) {
    v <- colmeta$var[[i]]
    is_num <- colmeta$is_num[[i]]
    stats <- try(describe_collectibles_stats_single_query(x, v, is_num = is_num, fast = fast),
                 silent = TRUE)
    if (inherits(stats, "try-error")) {
      failed <- c(failed, v)
    } else {
      out[[v]] <- stats
    }
  }

  if (length(failed)) {
    vals <- dplyr::collect(
      dplyr::select(x, dplyr::all_of(failed))
    )

    for (v in failed) {
      is_num <- colmeta$is_num[[which(colmeta$var == v)[[1]]]]
      out[[v]] <- describe_vector_stats(vals[[v]], is_num = is_num, fast = fast)
    }
  }

  out[colmeta$var]
}


describe_collectibles_stats_batched <- function(x, colmeta, fast = FALSE) {
  vars <- colmeta$var
  num_vars <- colmeta$var[colmeta$is_num]
  non_num_vars <- colmeta$var[!colmeta$is_num]

  exprs <- list(
    n = dplyr::n(),
    dplyr::across(
      dplyr::all_of(vars),
      ~ sum(is.na(.x), na.rm = TRUE),
      .names = "n_na__{.col}"
    )
  )
  if (!fast) {
    exprs <- c(exprs, list(
      dplyr::across(
        dplyr::all_of(vars),
        ~ dplyr::n_distinct(.x),
        .names = "n_distinct__{.col}"
      )
    ))
  }

  if (length(num_vars)) {
    exprs <- c(exprs, list(
      dplyr::across(dplyr::all_of(num_vars), ~ min(.x, na.rm = TRUE), .names = "min__{.col}"),
      dplyr::across(dplyr::all_of(num_vars), ~ mean(.x, na.rm = TRUE), .names = "mean__{.col}"),
      dplyr::across(dplyr::all_of(num_vars), ~ max(.x, na.rm = TRUE), .names = "max__{.col}"),
      dplyr::across(dplyr::all_of(num_vars), ~ stats::sd(.x, na.rm = TRUE), .names = "sd__{.col}")
    ))
    if (!fast) {
      exprs <- c(exprs, list(
        dplyr::across(
          dplyr::all_of(num_vars),
          ~ stats::median(.x, na.rm = TRUE),
          .names = "median__{.col}"
        )
      ))
    }
  }

  if (length(non_num_vars)) {
    exprs <- c(exprs, list(
      dplyr::across(
        dplyr::all_of(non_num_vars),
        ~ min(nchar(as.character(.x)), na.rm = TRUE),
        .names = "min__{.col}"
      ),
      dplyr::across(
        dplyr::all_of(non_num_vars),
        ~ mean(nchar(as.character(.x)), na.rm = TRUE),
        .names = "mean__{.col}"
      ),
      dplyr::across(
        dplyr::all_of(non_num_vars),
        ~ max(nchar(as.character(.x)), na.rm = TRUE),
        .names = "max__{.col}"
      ),
      dplyr::across(
        dplyr::all_of(non_num_vars),
        ~ stats::sd(nchar(as.character(.x)), na.rm = TRUE),
        .names = "sd__{.col}"
      )
    ))
    if (!fast) {
      exprs <- c(exprs, list(
        dplyr::across(
          dplyr::all_of(non_num_vars),
          ~ stats::median(nchar(as.character(.x)), na.rm = TRUE),
          .names = "median__{.col}"
        )
      ))
    }
  }

  summed <- dplyr::collect(dplyr::summarise(x, !!!exprs))

  n <- as.integer(summed$n[[1]])
  out <- lapply(seq_len(nrow(colmeta)), function(i) {
    v <- colmeta$var[[i]]
    data.frame(
      n = n,
      n_distinct = if (fast) as.integer(NA) else as.integer(summed[[paste0("n_distinct__", v)]][[1]]),
      n_na = as.integer(summed[[paste0("n_na__", v)]][[1]]),
      min = as.numeric(summed[[paste0("min__", v)]][[1]]),
      mean = as.numeric(summed[[paste0("mean__", v)]][[1]]),
      median = if (fast) as.numeric(NA) else as.numeric(summed[[paste0("median__", v)]][[1]]),
      max = as.numeric(summed[[paste0("max__", v)]][[1]]),
      sd = as.numeric(summed[[paste0("sd__", v)]][[1]])
    )
  })
  names(out) <- colmeta$var
  out
}


describe_collectibles_stats_single_query <- function(x, v, is_num, fast = FALSE) {
  if (is_num) {
    if (fast) {
      coerce_collectibles_stats(
        dplyr::collect(
          dplyr::summarise(
            x,
            n = dplyr::n(),
            n_na = sum(is.na(.data[[v]]), na.rm = TRUE),
            min = min(.data[[v]], na.rm = TRUE),
            mean = mean(.data[[v]], na.rm = TRUE),
            max = max(.data[[v]], na.rm = TRUE),
            sd = stats::sd(.data[[v]], na.rm = TRUE)
          )
        ),
        fast = fast
      )
    } else {
      coerce_collectibles_stats(
        dplyr::collect(
          dplyr::summarise(
            x,
            n = dplyr::n(),
            n_distinct = dplyr::n_distinct(.data[[v]]),
            n_na = sum(is.na(.data[[v]]), na.rm = TRUE),
            min = min(.data[[v]], na.rm = TRUE),
            mean = mean(.data[[v]], na.rm = TRUE),
            median = stats::median(.data[[v]], na.rm = TRUE),
            max = max(.data[[v]], na.rm = TRUE),
            sd = stats::sd(.data[[v]], na.rm = TRUE)
          )
        ),
        fast = fast
      )
    }
  } else {
    if (fast) {
      coerce_collectibles_stats(
        dplyr::collect(
          dplyr::summarise(
            x,
            n = dplyr::n(),
            n_na = sum(is.na(.data[[v]]), na.rm = TRUE),
            min = min(nchar(as.character(.data[[v]])), na.rm = TRUE),
            mean = mean(nchar(as.character(.data[[v]])), na.rm = TRUE),
            max = max(nchar(as.character(.data[[v]])), na.rm = TRUE),
            sd = stats::sd(nchar(as.character(.data[[v]])), na.rm = TRUE)
          )
        ),
        fast = fast
      )
    } else {
      coerce_collectibles_stats(
        dplyr::collect(
          dplyr::summarise(
            x,
            n = dplyr::n(),
            n_distinct = dplyr::n_distinct(.data[[v]]),
            n_na = sum(is.na(.data[[v]]), na.rm = TRUE),
            min = min(nchar(as.character(.data[[v]])), na.rm = TRUE),
            mean = mean(nchar(as.character(.data[[v]])), na.rm = TRUE),
            median = stats::median(nchar(as.character(.data[[v]])), na.rm = TRUE),
            max = max(nchar(as.character(.data[[v]])), na.rm = TRUE),
            sd = stats::sd(nchar(as.character(.data[[v]])), na.rm = TRUE)
          )
        ),
        fast = fast
      )
    }
  }
}


describe_collectibles_most_frequent <- function(x, vars, max_n = 3, skip_ones = TRUE, digits = 4) {
  out <- stats::setNames(rep("", length(vars)), vars)
  failed <- character()

  for (v in vars) {
    mc <- try(
      dplyr::collect(
        dplyr::slice_max(
          dplyr::count(x, .data[[v]]),
          n,
          n = max_n,
          with_ties = FALSE
        )
      ),
      silent = TRUE
    )
    if (inherits(mc, "try-error")) {
      failed <- c(failed, v)
      next
    }
    out[[v]] <- format_most_frequent(mc[[1]], mc[[2]], skip_ones = skip_ones, digits = digits)
  }

  if (length(failed)) {
    vals <- dplyr::collect(
      dplyr::select(x, dplyr::all_of(failed))
    )
    for (v in failed) {
      tc <- top_counts(vals[[v]], max_n = max_n)
      out[[v]] <- format_most_frequent(tc$values, tc$counts, skip_ones = skip_ones, digits = digits)
    }
  }

  out
}


describe_collectibles_colmeta <- function(x) {
  meta <- try(describe_collectibles_colmeta_from_arrow_schema(x), silent = TRUE)
  if (!inherits(meta, "try-error")) return(meta)

  proto <- dplyr::collect(utils::head(x, 0))
  vars <- colnames(proto)
  data.frame(
    var = vars,
    type = vapply(vars, function(v) class(proto[[v]])[[1]], character(1)),
    is_num = vapply(vars, function(v) is_numeric(proto[[v]]), logical(1)),
    stringsAsFactors = FALSE
  )
}


describe_collectibles_colmeta_from_arrow_schema <- function(x) {
  if (!is_arrow_collectible(x)) stop("Not an Arrow collectible")

  schema <- try(x$schema, silent = TRUE)
  if (inherits(schema, "try-error") || is.null(schema)) stop("No Arrow schema available")

  vars <- try(schema$names, silent = TRUE)
  fields <- try(schema$fields, silent = TRUE)
  if (inherits(vars, "try-error") || inherits(fields, "try-error")) {
    stop("Arrow schema metadata unavailable")
  }
  if (length(vars) != length(fields)) stop("Arrow schema mismatch")

  type_strings <- vapply(fields, function(field) {
    tt <- try(field$type$ToString(), silent = TRUE)
    if (inherits(tt, "try-error")) return(NA_character_)
    as.character(tt)
  }, character(1))
  if (anyNA(type_strings)) stop("Arrow type metadata unavailable")

  type <- vapply(type_strings, map_arrow_type_to_r_type, character(1))
  data.frame(
    var = vars,
    type = type,
    is_num = type %in% c("integer", "numeric", "POSIXct"),
    stringsAsFactors = FALSE
  )
}


is_arrow_collectible <- function(x) {
  any(class(x) %in% c("ArrowObject", "arrow_dplyr_query", "Dataset", "FileSystemDataset"))
}


map_arrow_type_to_r_type <- function(type_string) {
  tt <- tolower(type_string)
  if (grepl("dictionary", tt)) return("factor")
  if (grepl("timestamp|date|time", tt)) return("POSIXct")
  if (grepl("int", tt)) return("integer")
  if (grepl("float|double|decimal", tt)) return("numeric")
  if (grepl("bool", tt)) return("logical")
  if (grepl("utf8|string|binary", tt)) return("character")
  "character"
}

coerce_collectibles_stats <- function(stats, fast = FALSE) {
  if ("n" %in% names(stats)) stats$n <- as.integer(stats$n)
  if ("n_distinct" %in% names(stats)) stats$n_distinct <- as.integer(stats$n_distinct)
  if ("n_na" %in% names(stats)) stats$n_na <- as.integer(stats$n_na)

  stat_cols <- intersect(c("min", "mean", "median", "max", "sd"), names(stats))
  for (nm in stat_cols) stats[[nm]] <- suppressWarnings(as.numeric(stats[[nm]]))

  if (isTRUE(fast)) {
    stats$n_distinct <- as.integer(NA)
    stats$median <- as.numeric(NA)
  }

  stats
}


describe_vector_stats <- function(vv, is_num, fast = FALSE) {
  nz <- if (!is_num) nchar(as.character(vv))
  data.frame(
    n = length(vv),
    n_distinct = if (fast) NA_integer_ else length(unique(vv)),
    n_na = sum(is.na(vv)),
    min = as.numeric(min(if (is_num) vv else nz, na.rm = TRUE)),
    mean = as.numeric(mean(if (is_num) vv else nz, na.rm = TRUE)),
    median = if (fast) {
      NA_real_
    } else {
      as.numeric(stats::median(if (is_num) vv else nz, na.rm = TRUE))
    },
    max = as.numeric(max(if (is_num) vv else nz, na.rm = TRUE)),
    sd = as.numeric(stats::sd(if (is_num) vv else nz, na.rm = TRUE))
  )
}


describe_sql <- function(x, max_n = 3, skip_ones = TRUE, digits = 4, fast = FALSE) {
  describe_collectibles(x, max_n = max_n, skip_ones = skip_ones, digits = digits, fast = fast)
}


describe_arrow <- function(x, max_n = 3, skip_ones = TRUE, digits = 4, fast = FALSE) {
  describe_collectibles(x, max_n = max_n, skip_ones = skip_ones, digits = digits, fast = fast)
}
