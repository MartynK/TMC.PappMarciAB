#' Calculate the missing value among prop_need, prop_neg, and NPV
#'
#' This function calculates the missing parameter among \code{prop_need}, \code{prop_neg}, and \code{NPV} based on the relationship:
#' \deqn{NPV = prop\_need / prop\_neg}
#' At least two parameters must be provided, and exactly one must be \code{NA}.
#'
#' @param prop_need Numeric between 0 and 1. The proportion of subjects needing the test. Default is \code{0.75}.
#' @param prop_neg Numeric between 0 and 1. The proportion of subjects expected to have a negative test result.
#' @param NPV Numeric between 0 and 1. The Negative Predictive Value of the test.
#'
#' @return A list with all three parameters: \code{prop_need}, \code{prop_neg}, and \code{NPV}.
#'
#' @details
#' - If \code{NPV} is missing (\code{NA}), it is calculated as \code{NPV = prop_need / prop_neg}.
#' - If \code{prop_neg} is missing (\code{NA}), it is calculated as \code{prop_neg = prop_need / NPV}.
#' - If \code{prop_need} is missing (\code{NA}), it is calculated as \code{prop_need = NPV * prop_neg}.
#' - If more than one parameter is missing, or none are missing, an error is returned.
#' - All inputs should be numeric values between 0 and 1.
#'
#' @examples
#' # Calculate NPV when prop_need and prop_neg are given
#' calculate_missing_value(prop_need = 0.75, prop_neg = 0.2, NPV = NA)
#'
#' # Calculate prop_neg when prop_need and NPV are given
#' calculate_missing_value(prop_need = 0.75, prop_neg = NA, NPV = 0.9)
#'
#' # Calculate prop_need when prop_neg and NPV are given
#' calculate_missing_value(prop_need = NA, prop_neg = 0.2, NPV = 0.9)
#'
calculate_missing_value <- function(prop_need = 0.75, prop_neg = NA, NPV = NA) {
  # Create a vector of the inputs
  inputs <- c(prop_need = prop_need, prop_neg = prop_neg, NPV = NPV)

  # Count the number of NAs
  na_count <- sum(is.na(inputs))

  # Check that exactly one input is NA
  if (na_count != 1) {
    stop("Exactly one of prop_need, prop_neg, or NPV must be NA.")
  }

  # Check which one is NA and calculate it
  if (is.na(NPV)) {
    # Calculate NPV
    res <- (1-prop_need) / prop_neg
    names(res) <- "NPV"
  } else if (is.na(prop_neg)) {
    # Calculate prop_neg
    res <- (1-prop_need) / NPV
    names(res) <- "prop_neg"
  } else if (is.na((1-prop_need))) {
    # Calculate (1-prop_need)
    res <- NPV * prop_neg
    names(res) <- "prop_need"
  }

  # Return the results as a list
  return(res)
}

Calculate_missing_value <- Vectorize(calculate_missing_value, vectorize.args = c("prop_need", "prop_neg", "NPV"))
