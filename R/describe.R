#' Describes a dataset
#'
#' Note that the current version is in the beta stadium at best, that means the
#' R-native formats (data.frame, dplyr/tibble, or data.table) are a lot faster
#' than arrow or SQL-based datasets.
#'
#' @param x a dataset, either a [`data.frame`], [`dplyr::tibble`], [`data.table::data.table`],
#' [`arrow::arrow_table`], [`arrow::open_dataset`], or [`dplyr::tbl`] (SQL connection)
#' @param skip_ones logical, whether values that occur exactly once should be omitted from `most_frequent`
#' @param digits integer, number of digits to round numeric values in `most_frequent`
#'
#' @return a `data.frame`, `dplyr::tibble`, or `data.table::data.table` containing
#' a summary of the dataset given
#' @details
#' Numeric values in `most_frequent` are rounded to `digits` (default: 4).
#' If a variable has at most 1 distinct value, `most_frequent` is left empty.
#' By default, values with count 1 are omitted from `most_frequent`.
#' @export
#'
#' @seealso Similar to [skimr::skim()](https://cran.r-project.org/web/packages/skimr/vignettes/skimr.html),
#'   [summarytools::dfSummary()](https://cran.r-project.org/web/packages/summarytools/vignettes/introduction.html#data-frame-summaries-dfsummary),
#'   and [gtExtras::gt_plt_summary()](https://jthomasmock.github.io/gtExtras/reference/gt_plt_summary.html)
#'
#' @examples
#' describe(mtcars)
describe <- function(x, skip_ones = TRUE, digits = 4) {

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
    describe_base_r(x, skip_ones = skip_ones, digits = digits)
  } else if (backend == "dplyr") {
    describe_dplyr(x, skip_ones = skip_ones, digits = digits)
  } else if (backend == "data.table") {
    describe_data.table(x, skip_ones = skip_ones, digits = digits)
  } else if (backend == "collectibles") {
    if ("tbl_sql" %in% class(x)) {
      describe_sql(x, skip_ones = skip_ones, digits = digits)
    } else if ("ArrowObject" %in% class(x)) {
      describe_arrow(x, skip_ones = skip_ones, digits = digits)
    }
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

# x <- mtcars
describe_base_r <- function(x, max_n = 3, skip_ones = TRUE, digits = 4) {
  ll <- lapply(
    seq(ncol(x)),
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
        n_distinct = length(unique(v)),
        n_na = sum(is.na(v)),
        most_frequent = format_most_frequent(tc$values, tc$counts, skip_ones = skip_ones, digits = digits),

        min = as.numeric(min(if (is_num) v else nz, na.rm = TRUE)),
        mean = as.numeric(mean(if (is_num) v else nz, na.rm = TRUE)),
        median = as.numeric(stats::median(if (is_num) v else nz, na.rm = TRUE)),
        max = as.numeric(max(if (is_num) v else nz, na.rm = TRUE)),
        sd = as.numeric(stats::sd(if (is_num) v else nz, na.rm = TRUE))
      )
    }
  )

  do.call(rbind, ll)
}

# x <- mtcars |> tibble::as_tibble()
describe_dplyr <- function(x, max_n = 3, skip_ones = TRUE, digits = 4) {
  ll <- lapply(
    names(x),
    function(v) {
      vv <- x[[v]]
      tc <- top_counts(vv, max_n = max_n)

      type <- class(vv)[[1]]
      is_num <- is_numeric(vv)
      mf <- format_most_frequent(tc$values, tc$counts, skip_ones = skip_ones, digits = digits)

      nz <- if (!is_num) nchar(as.character(x[[v]]))
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
  )

  dplyr::bind_rows(ll)
}

# x <- mtcars |> data.table::as.data.table()
describe_data.table <- function(x, max_n = 3, skip_ones = TRUE, digits = 4) {
  ll <- lapply(
    names(x),
    function(v) {
      vv <- x[[v]]
      tc <- top_counts(vv, max_n = max_n)

      type <- class(vv)[[1]]
      is_num <- is_numeric(vv)
      mf <- format_most_frequent(tc$values, tc$counts, skip_ones = skip_ones, digits = digits)

      nz <- if (!is_num) nchar(as.character(x[[v]]))

      x[, .(
        var = v,
        type = type,
        n = .N,
        n_distinct = data.table::uniqueN(get(v)),
        n_na = sum(is.na(get(v))),
        most_frequent = mf,

        min = as.numeric(min(if (is_num) get(v) else nz, na.rm = TRUE)),
        mean = as.numeric(mean(if (is_num) get(v) else nz, na.rm = TRUE)),
        median = as.numeric(stats::median(if (is_num) get(v) else nz, na.rm = TRUE)),
        max = as.numeric(max(if (is_num) get(v) else nz, na.rm = TRUE)),
        sd = as.numeric(stats::sd(if (is_num) get(v) else nz, na.rm = TRUE))
      )]
    }
  )

  data.table::rbindlist(ll)
}


# RSQLite, duckdb etc
describe_sql <- function(x, max_n = 3, skip_ones = TRUE, digits = 4) {
  ll <- lapply(colnames(x), function(v) {
    mc <- x |>
      dplyr::count(!!rlang::sym(v)) |>
      dplyr::slice_max(n, n = max_n, with_ties = FALSE) |>
      dplyr::collect()

    vv <- x |>
      dplyr::select(dplyr::all_of(v)) |>
      dplyr::collect() |>
      dplyr::pull(1)

    type <- class(vv)[[1]]
    is_num <- is_numeric(vv)
    mf <- format_most_frequent(mc[[1]], mc[[2]], skip_ones = skip_ones, digits = digits)

    r <- dplyr::tibble(
      var = v,
      type = type,
      n = length(vv),
      n_distinct = length(unique(vv)),
      n_na = sum(is.na(vv)),
      most_frequent = mf
    )
    nz <- if (!is_num) nchar(as.character(vv))
    rr <- dplyr::tibble(
      min = as.numeric(min(if (is_num) vv else nz, na.rm = TRUE)),
      mean = as.numeric(mean(if (is_num) vv else nz, na.rm = TRUE)),
      median = as.numeric(stats::median(if (is_num) vv else nz, na.rm = TRUE)),
      max = as.numeric(max(if (is_num) vv else nz, na.rm = TRUE)),
      sd = as.numeric(stats::sd(if (is_num) vv else nz, na.rm = TRUE))
    )

    dplyr::bind_cols(r, rr)
  })

  dplyr::bind_rows(ll)
}

# arrow::write_parquet(nycflights13::flights, "flights.parquet")
# x <- arrow::open_dataset("flights.parquet")
describe_arrow <- function(x, max_n = 3, skip_ones = TRUE, digits = 4) {
  # if x is a dbplyr connection string
  ll <- lapply(colnames(x), function(v) {
    mc <- try(
      x |>
        dplyr::count(!!rlang::sym(v)) |>
        dplyr::slice_max(n, n = max_n, with_ties = FALSE) |>
        dplyr::collect(),
      silent = TRUE
    )

    vv <- x |>
      dplyr::select(dplyr::all_of(v)) |>
      dplyr::collect() |>
      dplyr::pull(1)
    if (inherits(mc, "try-error")) {
      tc <- top_counts(vv, max_n = max_n)
      mc <- data.frame(v = tc$values, n = tc$counts)
    }

    type <- class(vv)[[1]]
    is_num <- is_numeric(vv)
    mf <- format_most_frequent(mc[[1]], mc[[2]], skip_ones = skip_ones, digits = digits)

    r <- dplyr::tibble(
      var = v,
      type = type,
      n = length(vv),
      n_distinct = length(unique(vv)),
      n_na = sum(is.na(vv)),
      most_frequent = mf
    )
    nz <- if (!is_num) nchar(as.character(vv))
    rr <- dplyr::tibble(
      min = as.numeric(min(if (is_num) vv else nz, na.rm = TRUE)),
      mean = as.numeric(mean(if (is_num) vv else nz, na.rm = TRUE)),
      median = as.numeric(stats::median(if (is_num) vv else nz, na.rm = TRUE)),
      max = as.numeric(max(if (is_num) vv else nz, na.rm = TRUE)),
      sd = as.numeric(stats::sd(if (is_num) vv else nz, na.rm = TRUE))
    )

    dplyr::bind_cols(r, rr)
  })

  dplyr::bind_rows(ll)
}
