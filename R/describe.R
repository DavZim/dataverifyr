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
#' [skimr::skim()](https://cran.r-project.org/web/packages/skimr/vignettes/skimr.html),
#' [summarytools::dfSummary()](https://cran.r-project.org/web/packages/summarytools/vignettes/introduction.html#data-frame-summaries-dfsummary),
#' and [gtExtras::gt_plt_summary()](https://jthomasmock.github.io/gtExtras/reference/gt_plt_summary.html)
#'
#' @examples
#' describe(mtcars)
describe <- function(x, skip_ones = TRUE, digits = 4, top_n = 3, fast = FALSE) {

  if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n) || top_n < 1) {
    stop("`top_n` must be a single number >= 1.")
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

# eg x <- mtcars |> tibble::as_tibble()
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
        x |>
          dplyr::summarise(
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
        x |>
          dplyr::summarise(
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

# eg x <- mtcars |> data.table::as.data.table()
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
  proto <- dplyr::collect(utils::head(x, 0))

  ll <- lapply(colnames(x), function(v) {
    vv <- NULL
    get_v <- function() {
      if (is.null(vv)) {
        vv <<- x |>
          dplyr::select(dplyr::all_of(v)) |>
          dplyr::collect() |>
          dplyr::pull(1)
      }
      vv
    }

    type <- class(proto[[v]])[[1]]
    is_num <- is_numeric(proto[[v]])

    stats <- try(describe_collectibles_stats(x, v, is_num, fast = fast), silent = TRUE)
    if (inherits(stats, "try-error")) {
      vals <- get_v()
      stats <- describe_vector_stats(vals, is_num = is_num, fast = fast)
    }
    if (fast) {
      stats$n_distinct <- as.integer(NA)
      stats$median <- as.numeric(NA)
    }

    mf <- NA_character_
    if (!fast) {
      mc <- try(
        x |>
          dplyr::count(.data[[v]]) |>
          dplyr::slice_max(n, n = max_n, with_ties = FALSE) |>
          dplyr::collect(),
        silent = TRUE
      )
      if (inherits(mc, "try-error")) {
        tc <- top_counts(get_v(), max_n = max_n)
        mc <- data.frame(v = tc$values, n = tc$counts)
      }
      mf <- format_most_frequent(mc[[1]], mc[[2]], skip_ones = skip_ones, digits = digits)
    }

    dplyr::tibble(
      var = v,
      type = type,
      n = stats$n,
      n_distinct = stats$n_distinct,
      n_na = stats$n_na,
      most_frequent = mf,
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
  if (is_num) {
    if (fast) {
      dplyr::summarise(
        x,
        n = dplyr::n(),
        n_na = sum(is.na(.data[[v]]), na.rm = TRUE),
        min = as.numeric(min(.data[[v]], na.rm = TRUE)),
        mean = as.numeric(mean(.data[[v]], na.rm = TRUE)),
        max = as.numeric(max(.data[[v]], na.rm = TRUE)),
        sd = as.numeric(stats::sd(.data[[v]], na.rm = TRUE))
      ) |>
        dplyr::collect()
    } else {
      dplyr::summarise(
        x,
        n = dplyr::n(),
        n_distinct = dplyr::n_distinct(.data[[v]]),
        n_na = sum(is.na(.data[[v]]), na.rm = TRUE),
        min = as.numeric(min(.data[[v]], na.rm = TRUE)),
        mean = as.numeric(mean(.data[[v]], na.rm = TRUE)),
        median = as.numeric(stats::median(.data[[v]], na.rm = TRUE)),
        max = as.numeric(max(.data[[v]], na.rm = TRUE)),
        sd = as.numeric(stats::sd(.data[[v]], na.rm = TRUE))
      ) |>
        dplyr::collect()
    }
  } else {
    if (fast) {
      dplyr::summarise(
        x,
        n = dplyr::n(),
        n_na = sum(is.na(.data[[v]]), na.rm = TRUE),
        min = as.numeric(min(nchar(as.character(.data[[v]])), na.rm = TRUE)),
        mean = as.numeric(mean(nchar(as.character(.data[[v]])), na.rm = TRUE)),
        max = as.numeric(max(nchar(as.character(.data[[v]])), na.rm = TRUE)),
        sd = as.numeric(stats::sd(nchar(as.character(.data[[v]])), na.rm = TRUE))
      ) |>
        dplyr::collect()
    } else {
      dplyr::summarise(
        x,
        n = dplyr::n(),
        n_distinct = dplyr::n_distinct(.data[[v]]),
        n_na = sum(is.na(.data[[v]]), na.rm = TRUE),
        min = as.numeric(min(nchar(as.character(.data[[v]])), na.rm = TRUE)),
        mean = as.numeric(mean(nchar(as.character(.data[[v]])), na.rm = TRUE)),
        median = as.numeric(stats::median(nchar(as.character(.data[[v]])), na.rm = TRUE)),
        max = as.numeric(max(nchar(as.character(.data[[v]])), na.rm = TRUE)),
        sd = as.numeric(stats::sd(nchar(as.character(.data[[v]])), na.rm = TRUE))
      ) |>
        dplyr::collect()
    }
  }
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
