% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ruleset_construction.R
\name{dataverifyr_plus}
\alias{dataverifyr_plus}
\alias{datavarifyr_plus}
\alias{+.ruleset}
\alias{+.rule}
\title{Add Rules and Rulesets Together}
\usage{
datavarifyr_plus(a, b)

\method{+}{ruleset}(a, b)

\method{+}{rule}(a, b)
}
\arguments{
\item{a}{the first ruleset you wish to add}

\item{b}{the second ruleset you wish to add}
}
\description{
\itemize{
\item allows you to add rules and rulesets into larger rulesets. This
can be useful if you want to create a ruleset for a dataset out of checks
for other datasets.
}
}
