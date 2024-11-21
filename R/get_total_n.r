#' Calculate the total number of subjects required for the study
#'
#' This function computes the adjusted total number of subjects needed for a study, accounting for the fraction of positive tests evaluated, dropout rate, and whether all positive cases are evaluated. It ensures the total number is rounded up to the nearest even integer to accommodate study design requirements.
#'
#' @param n_total Numeric. The total number of subjects in the initial sample.
#' @param frac_tst Numeric between 0 and 1. The fraction of positive test cases that are evaluated (e.g., 0.5 means 50% of positive cases are evaluated).
#' @param tp_a Numeric. The number of true positives in group A. If `NA`, it will be estimated based on `prop_neg_a`.
#' @param fp_a Numeric. The number of false positives in group A. If `NA`, it will be estimated based on `prop_neg_a`.
#' @param prop_neg_a Numeric between 0 and 1. The proportion of negative cases in group A.
#' @param evaled Logical. Indicates whether all positive cases are evaluated (`TRUE`) or only a fraction (`FALSE`). Default is `FALSE`.
#' @param dropout Numeric between 0 and 1. The expected dropout rate (e.g., 0.1 for 10% dropout). Default is `0.1`.
#'
#' @return Numeric. The adjusted total number of subjects required, rounded up to the nearest even integer.
#'
#' @details
#' - If `tp_a` or `fp_a` are missing (`NA`), they are estimated assuming an equal split between true positives and false positives based on `prop_neg_a` and `n_total`.
#' - The function adjusts the total number of subjects to account for the fraction of positive tests evaluated and expected dropout rate.
#' - The final result is rounded up to the nearest even integer to ensure balanced group sizes or other study design considerations.
#'
#' @examples
#' # Example with all parameters provided
#' get_total_n(n_total = 100, frac_tst = 0.5, tp_a = 20, fp_a = 5, prop_neg_a = 0.2)
#'
#' # Example with tp_a and fp_a missing (will be estimated)
#' get_total_n(n_total = 100, frac_tst = 0.5, tp_a = NA, fp_a = NA, prop_neg_a = 0.2)
#'
#' # Example where all positive cases are evaluated
#' get_total_n(n_total = 100, frac_tst = 1, tp_a = NA, fp_a = NA, prop_neg_a = 0.2, evaled = TRUE)
#'
get_total_n <- function(n_total, frac_tst, tp_a, fp_a, prop_neg_a,
                        evaled = FALSE,
                        dropout = .1) {

  # Check if 'tp_a' or 'fp_a' are missing (NA)
  # If so, set the positives as half true half false positives (doesnt really matter)
  if (is.na(tp_a[1]) | is.na(fp_a[1])) {
    tp_a <- (1 - prop_neg_a) * n_total/2
    fp_a <- (1 - prop_neg_a) * n_total/2
  }

  # Calculate the initial total number of subjects required
  # if 'evaled' no. subs. are requested, calc. only the fraction of positive cases
  if (evaled == FALSE) {
    res <- (n_total + (tp_a + fp_a) / (frac_tst))
  } else {
    res <- (n_total + (tp_a + fp_a) )
  }

  # Adjust for dropout rate and ensure the total number is an even integer

  res <- res %>%
    {./(1-dropout)} %>%
    ceiling() %>%
    {./2} %>%
    ceiling() %>%
    {.*2}

  # Return the calculated total number of subjects required
  return(res)
}
