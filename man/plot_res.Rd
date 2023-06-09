% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualization.R
\name{plot_res}
\alias{plot_res}
\title{Visualize the results of a data validation}
\usage{
plot_res(
  res,
  main = "Verification Results per Rule",
  colors = c(pass = "#308344", fail = "#E66820"),
  labels = TRUE,
  table = TRUE
)
}
\arguments{
\item{res}{a data.frame as returned by \code{\link[=check_data]{check_data()}}}

\item{main}{the title of the plot}

\item{colors}{a named list of colors, with the names pass and fail}

\item{labels}{whether the values should be displayed on the barplot}

\item{table}{show a table in the legend with the values}
}
\value{
a base r plot
}
\description{
Visualize the results of a data validation
}
\examples{
rs <- ruleset(
  rule(Ozone > 0 & Ozone < 120, allow_na = TRUE), # some mising values and > 120
  rule(Solar.R > 0, allow_na = TRUE),
  rule(Solar.R < 200, allow_na = TRUE),
  rule(Wind > 10),
  rule(Temp < 100)
)

res <- check_data(airquality, rs)
plot_res(res)
}
