#' Visualize the results of a data validation
#'
#' @param res a data.frame as returned by [`check_data()`]
#' @param main the title of the plot
#' @param colors a named list of colors, with the names pass and fail
#' @param labels whether the values should be displayed on the barplot
#' @param table show a table in the legend with the values
#'
#' @return a base r plot
#' @export
#'
#' @examples
#' rs <- ruleset(
#'   rule(Ozone > 0 & Ozone < 120, allow_na = TRUE), # some mising values and > 120
#'   rule(Solar.R > 0, allow_na = TRUE),
#'   rule(Solar.R < 200, allow_na = TRUE),
#'   rule(Wind > 10),
#'   rule(Temp < 100)
#' )
#'
#' res <- check_data(airquality, rs)
#' plot_res(res)
plot_res <- function(res,
                     main = "Verification Results per Rule",
                     colors = c(pass = "#308344", fail = "#E66820"),
                     labels = TRUE,
                     table = TRUE) {
  stopifnot(is.data.frame(res))
  stopifnot(all(c("name", "pass", "fail", "tests") %in% names(res)))

  n <- nrow(res)
  r <- res[rev(seq(n)), ]

  op <- par(mai = c(1.5, 2, 1, 1), xpd = TRUE, family = "mono")

  bp <- barplot(
    matrix(c(r$pass / r$tests, r$fail / r$tests), ncol = nrow(r), byrow = TRUE),
    names.arg = r$name, las = 1,
    col = colors[c("pass", "fail")],
    main = main,
    border = NA,
    cex.names = 0.75,
    axes = FALSE,
    horiz = TRUE
  )

  if (labels) {
    # add pass rates
    p <- r$pass / r$tests
    text(p / 2, bp, labels = sprintf("%s%% (%s)", round_down(100 * p),
                                     pretty_val(r$pass)),
         cex = 0.75, col = "white")

    # add failure rate
    f <- r$fail / r$tests
    text(p[f != 0] + f[f != 0] / 2, bp[f != 0],
         labels = sprintf("%s%% (%s)", round_down(100 * f[f != 0]),
                          pretty_val(r$fail[f != 0])),
         cex = 0.75, col = "white")
  }
  labs <- seq(0, 1, length.out = 5)
  axis(side = 1, at = labs, labels = paste0(labs * 100, "%"), cex.axis = 0.7)

  if (table) {
    legend(
      "bottomright", inset = c(0, -0.4),
      ncol = 4L, cex = 0.5,
      legend = c(
        "", res$name,
        "Pass", pretty_val(res$pass),
        "Fail", pretty_val(res$fail),
        "Total", pretty_val(res$tests)
      ),
      text.col = c(rep("black", n + 1), # name
                   rep(colors["pass"], n + 1), # pass
                   rep(colors["fail"], n + 1), # fail
                   rep("darkgray", n + 1)) # total
    )
  }

  par(op)
}

# x <- 99.999999999 to 99.99
round_down <- function(x, digits = 2) {
  floor(x * 10^digits) / 10^digits
}

pretty_val <- function(x, digits = 2, digits_small = FALSE, align = TRUE) {
  brks <- c("bln" = 1e9, "mln" = 1e6, "k" = 1e3, " " = 1)
  fmt <- paste0("%.", digits, "f%s")
  r <- as.character(x)

  for (i in seq_along(brks)) {
    nn <- names(brks)[[i]]

    r <- ifelse(abs(x) %/% brks[[i]] > 0 & abs(x) %/% brks[[i]] < 1000,
                sprintf(fmt, x / brks[[i]], names(brks)[[i]]),
                ifelse(x == 0, sprintf(fmt, x, ""), r))
  }
  r <- trimws(r)
  if (align) {
    fmt <- sprintf("%%%is", max(nchar(r)))
    r <- sprintf(fmt, r)
  }
  if (!digits_small) {
    # removes the digits for values without prefix...
    r <- gsub("\\.0+$", "", r)
  }
  r
}
