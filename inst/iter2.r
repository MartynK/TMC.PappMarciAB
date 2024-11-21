# Load necessary libraries
library(dplyr)

# Necessary parameters
n_total            <- 100
prop_need          <- 0.5
ab_choice_sensitiv <- 0.7
ab_choice_spec     <- 0.9


# Calculate the contingency table

p <- n_total * prop_need
n <- n_total - p
tp <- ab_choice_sensitiv * p
fp <- p - tp
tn <- ab_choice_spec * n
fn <- n - tn

validate <- (tp+tn+fp+fn) == n_total

# add rounding error
p_r  <- round(p)
n_r  <- n_total - p_r
tp_r <- round(ab_choice_sensitiv * p_r)
fp_r <- p_r - tp_r
tn_r <- round(ab_choice_spec * n_r)
fn_r <- n_r - tn_r

validate_r <- (tp_r+tn_r+fp_r+fn_r) == n_total

####
# Add the observable effect of AB therapy


ab_effectiveness   <- 0.9

abpoz_noinf_tp <- round(tp_r * ab_effectiveness) # pts who would have gotten sick
abpoz_noinf_tn <- fp_r                           # pts whou had unnec. treatment
abpoz_yesinf   <- (tp_r - abpoz_noinf_tp)        # pts who had trt. but are sick
abneg_noinf    <- tn_r                           # pts who had no trt and no need
abneg_yesinf   <- fn_r                           # pts who had no trt but are sick

