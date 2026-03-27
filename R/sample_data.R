#' Sample Orders Dataset for Examples and Tests
#'
#' A small, human-readable dataset with mixed column types, missing values, and
#' one datetime column. It is designed for documentation examples and unit tests.
#'
#' @format A data frame with 8 rows and 6 variables:
#' \describe{
#'   \item{order_id}{Integer order identifier.}
#'   \item{customer_tier}{Character tier (`"bronze"`, `"silver"`, `"gold"`, etc), includes one `NA`.}
#'   \item{amount}{Numeric order amount, includes one negative value and one `NA`.}
#'   \item{paid}{Logical payment flag, includes one `NA`.}
#'   \item{payment_method}{Character payment method, includes one `NA`.}
#'   \item{order_time}{`POSIXct` order timestamp in UTC, includes one `NA`.}
#' }
#' @export
sample_data <- data.frame(
  order_id = as.integer(1:8),
  customer_tier = c("gold", "silver", "bronze", "gold", NA, "silver", "bronze", "unknown"),
  amount = c(120.50, 80.00, -5.00, 320.25, 45.10, NA, 0.00, 99.99),
  paid = c(TRUE, TRUE, FALSE, TRUE, FALSE, NA, TRUE, TRUE),
  payment_method = c("card", "cash", "none", "card", "none", "card", NA, "none"),
  order_time = as.POSIXct(c(
    "2025-01-01 09:00:00", "2025-01-02 10:30:00", "2025-01-03 12:15:00",
    "2025-01-04 15:45:00", NA, "2025-01-06 08:10:00",
    "2025-01-07 17:20:00", "2025-01-08 11:05:00"
  ), tz = "UTC"),
  stringsAsFactors = FALSE
)
