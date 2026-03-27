#' Describes a dataset
#'
#' Note that the current version is in the beta stadium at best, that means the
#' R-native formats (data.frame, dplyr/tibble, or data.table) are a lot faster
#' than arrow or SQL-based datasets.
#'
#' @param x a dataset, either a [`data.frame`], [`dplyr::tibble`], [`data.table::data.table`],
#' [`arrow::arrow_table`], [`arrow::open_dataset`], or [`dplyr::tbl`] (SQL connection)
#'
#' @return a `data.frame`, `dplyr::tibble`, or `data.table::data.table` containing
#' a summary of the dataset given
#' @export
#'
#' @seealso Similar to [skimr::skim()](https://cran.r-project.org/web/packages/skimr/vignettes/skimr.html),
#'   [summarytools::dfSummary()](https://cran.r-project.org/web/packages/summarytools/vignettes/introduction.html#data-frame-summaries-dfsummary),
#'   and [gtExtras::gt_plt_summary()](https://jthomasmock.github.io/gtExtras/reference/gt_plt_summary.html)
#'
#' @examples
#' describe(mtcars)
describe <- function(x) {

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
    describe_base_r(x)
  } else if (backend == "dplyr") {
    describe_dplyr(x)
  } else if (backend == "data.table") {
    describe_data.table(x)
  } else if (backend == "collectibles") {
    if ("tbl_sql" %in% class(x)) {
      describe_sql(x)
    } else if ("ArrowObject" %in% class(x)) {
      describe_arrow(x)
    }
  } else {
    stop(sprintf("Could not detect backend to describe %s", paste(class(x), collapse = ", ")))
  }
}


# internal function to see which values should use the min/max etc part
is_numeric <- function(v) {
  any(class(v) %in% c("integer", "numeric", "POSIXt"))
}

# x <- mtcars
describe_base_r <- function(x, max_n = 3) {
  ll <- lapply(
    seq(ncol(x)),
    function(i) {
      v <- x[[i]]
      type <- class(v)[[1]]
      is_num <- is_numeric(v)

      tbl <- table(v)
      uv <- unique(v)
      tab <- tabulate(match(v, uv))
      tab_max <- which(tab == max(tab))
      # get the indices of the three highest counts
      od <- order(tab, decreasing = TRUE)[seq(min(max_n, length(tab)))]

      nz <- if (!is_num) nchar(as.character(v))

      data.frame(
        var = names(x)[[i]],
        type = type,
        n = length(v),
        n_distinct = length(unique(v)),
        n_na = sum(is.na(v)),
        most_frequent = paste(sprintf("%s (%s)", uv[od], tab[od]),
                              collapse = ", "),

        min = as.numeric(min(if (is_num) v else nz, na.rm = TRUE)),
        mean = as.numeric(mean(if (is_num) v else nz, na.rm = TRUE)),
        median = as.numeric(median(if (is_num) v else nz, na.rm = TRUE)),
        max = as.numeric(max(if (is_num) v else nz, na.rm = TRUE)),
        sd = as.numeric(sd(if (is_num) v else nz, na.rm = TRUE))
      )
    }
  )

  do.call(rbind, ll)
}

# x <- mtcars |> tibble::as_tibble()
describe_dplyr <- function(x, max_n = 3) {
  ll <- lapply(
    names(x),
    function(v) {
      mc <- x |>
        dplyr::count(.data[[v]]) |>
        dplyr::slice_max(n, n = max_n, with_ties = FALSE)

      type <- class(mc[[1]])[[1]]
      is_num <- is_numeric(mc[[1]])
      mf <- paste(sprintf("%s (%s)", mc[[1]], mc[[2]]), collapse = ", ")

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
          median = as.numeric(median(if (is_num) .data[[v]] else nz, na.rm = TRUE)),
          max = as.numeric(max(if (is_num) .data[[v]] else nz, na.rm = TRUE)),
          sd = as.numeric(sd(if (is_num) .data[[v]] else nz, na.rm = TRUE))
        )
    }
  )

  dplyr::bind_rows(ll)
}

# x <- mtcars |> data.table::as.data.table()
describe_data.table <- function(x, max_n = 3) {
  ll <- lapply(
    names(x),
    function(v) {
      mc <- x[, .(n = .N), by = v][order(n, decreasing = TRUE)][seq(max_n)]

      type <- class(mc[[1]])[[1]]
      is_num <- is_numeric(mc[[1]])
      mf <- paste(sprintf("%s (%s)", mc[[1]], mc[[2]]), collapse = ", ")

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
        median = as.numeric(median(if (is_num) get(v) else nz, na.rm = TRUE)),
        max = as.numeric(max(if (is_num) get(v) else nz, na.rm = TRUE)),
        sd = as.numeric(sd(if (is_num) get(v) else nz, na.rm = TRUE))
      )]
    }
  )

  data.table::rbindlist(ll)
}


# RSQLite, duckdb etc
describe_sql <- function(x, max_n = 3) {
  ll <- lapply(names(x), function(v) {
    mc <- x |>
      dplyr::count(.data[[v]]) |>
      dplyr::slice_max(n, n = max_n, with_ties = FALSE) |>
      dplyr::collect()

    type <- class(mc[[1]])[[1]]
    is_num <- is_numeric(mc[[1]])
    mf <- paste(sprintf("%s (%s)", mc[[1]], mc[[2]]), collapse = ", ")
    nn <- x |>
      dplyr::distinct(.data[[v]]) |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::collect()
    nna <- x |> dplyr::filter(is.na(.data[[v]])) |> dplyr::collect() |> nrow()

    r <- dplyr::tibble(
      var = v,
      type = type,
      n_distinct = nn[[1]],
      n_na = nna,
      most_frequent = mf
    )

    xx <- x |>
      dplyr::select(dplyr::all_of(v)) |>
      dplyr::rename(x := dplyr::all_of(v))
    if (!is_num) xx <- xx |> dplyr::mutate(x = nchar(as.character(x)))


    rr <- try(
      xx |>
        dplyr::summarise(
          min = min(x, na.rm = TRUE),
          mean = mean(x, na.rm = TRUE),
          median = median(x, na.rm = TRUE),
          max = max(x, na.rm = TRUE),
          sd = sd(x, na.rm = TRUE)
        ) |>
        dplyr::collect(),
      silent = TRUE
    )
    if (inherits(rr, "try-error")) {
      rr <- dplyr::tibble(
        min = NA_real_, mean = NA_real_, median = NA_real_, max = NA_real_,
        sd = NA_real_
      )
    }

    dplyr::bind_cols(r, rr)
  })

  dplyr::bind_rows(ll)
}

# arrow::write_parquet(nycflights13::flights, "flights.parquet")
# x <- arrow::open_dataset("flights.parquet")
describe_arrow <- function(x, max_n = 3) {
  # if x is a dbplyr connection string
  ll <- lapply(names(x), function(v) {
    mc <- x |>
      dplyr::count(.data[[v]]) |>
      dplyr::slice_max(n, n = max_n, with_ties = FALSE) |>
      dplyr::collect()

    type <- class(mc[[1]])[[1]]
    is_num <- is_numeric(mc[[1]])
    mf <- paste(sprintf("%s (%s)", mc[[1]], mc[[2]]), collapse = ", ")
    nn <- x |>
      dplyr::distinct(.data[[v]]) |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::collect()
    nna <- x |> dplyr::filter(is.na(.data[[v]])) |> nrow()

    r <- dplyr::tibble(
      var = v,
      type = type,
      n_distinct = nn[[1]],
      n_na = nna,
      most_frequent = mf
    )

    if (is_num) {
      xx <- x |> dplyr::transmute(x = get(v))
    } else {
      xx <- x |> dplyr::transmute(x = nchar(as.character(get(v))))
    }

    suppressWarnings({
      rr <- try(
        xx |>
          dplyr::summarise(
            min = min(x, na.rm = TRUE),
            mean = mean(x, na.rm = TRUE),
            median = median(x, na.rm = TRUE),
            max = max(x, na.rm = TRUE),
            sd = sd(x, na.rm = TRUE)
          ) |>
          dplyr::collect(),
        silent = TRUE)
    })
    if (inherits(rr, "try-error")) {
      rr <- dplyr::tibble(
        min = NA_real_, mean = NA_real_, median = NA_real_, max = NA_real_,
        sd = NA_real_
      )
    }

    dplyr::bind_cols(r, rr)
  })

  dplyr::bind_rows(ll)
}
