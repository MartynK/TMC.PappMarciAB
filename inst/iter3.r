# This iteration can simulate n trials about an arm with error
# Returns two values for abpoz_noinf category and the 'detection' of them is
# an as of yet open question

# Load necessary libraries
library(dplyr)

# Necessary parameters
n_simulations <- 1000
n_total            <- 100
prop_need          <- 0.5
ab_choice_sensitiv <- 0.7325
ab_choice_spec     <- 0.87563

ab_eff <- 0.9

# Expert team aims to find the *SPURIOUS TREATED CASES*
expert_sens <- 0.9
expert_spec <- 0.8

# Function to calculate the contingency table
calculate_contingency_table <- function(n_total = 100,
                                        prop_need = 0.5,
                                        ab_choice_sensitiv = 0.75,
                                        ab_choice_spec = 0.875,
                                        rounding = FALSE) {
  if (rounding) {
    p <- round(n_total * prop_need)
    n <- n_total - p
    tp <- round(ab_choice_sensitiv * p)
    fp <- p - tp
    tn <- round(ab_choice_spec * n)
    fn <- n - tn
  } else {
    p <- n_total * prop_need
    n <- n_total - p
    tp <- ab_choice_sensitiv * p
    fp <- p - tp
    tn <- ab_choice_spec * n
    fn <- n - tn
  }

  validate <- (tp + tn + fp + fn) == n_total

  list(tp = tp, fp = fp, tn = tn, fn = fn, validate = validate)
}

# Function to calculate the observable effect of AB therapy
calculate_ab_effect <- function(cont_table,
                                ab_effectiveness = 0.9) {
  tp_r <- cont_table$tp
  fp_r <- cont_table$fp
  tn_r <- cont_table$tn
  fn_r <- cont_table$fn


  abpoz_noinf_tp <- rbinom(1,max(round(tp_r),0), ab_effectiveness) # pts who would have gotten sick, but didnt
  abpoz_noinf_fp <- fp_r                           # pts who had unnecessary treatment
  abpoz_yesinf   <- (tp_r - abpoz_noinf_tp)        # pts who had treatment but are still sick
  abneg_noinf    <- tn_r                           # pts who had no treatment and no need
  abneg_yesinf   <- fn_r                           # pts who had no treatment but are sick

  list(abpoz_noinf_tp = abpoz_noinf_tp,
       abpoz_noinf_fp = abpoz_noinf_fp,
       abpoz_yesinf = abpoz_yesinf,
       abneg_noinf = abneg_noinf,
       abneg_yesinf = abneg_yesinf)
}


# Function to simulate a contingency table with error
simulate_contingency_table <- function(expected_tbl) {

  # get the table values, dis the negative values
  tab_val <- expected_tbl %>% unlist %>% pmax(0)

  # Total number of observations in the table
  n_total <- sum(tab_val)

  # Convert the expected counts into probabilities
  expected_probs <- as.vector(tab_val) / n_total

  # Simulate counts using multinomial distribution
  simulated_counts <- rmultinom(n = 1, size = n_total, prob = expected_probs)

  # Convert back to named list
  simulated_tbl <- data.frame(
    abpoz_noinf_tp = simulated_counts[1],
    abpoz_noinf_fp = simulated_counts[2],
    abpoz_yesinf = simulated_counts[3],
    abneg_noinf = simulated_counts[4],
    abneg_yesinf = simulated_counts[5]
  )

  return(simulated_tbl)
}

# Simulate multiple AB effect tables
simulated_ab_effects <- calculate_contingency_table(n_total, prop_need, ab_choice_sensitiv, ab_choice_spec, rounding = FALSE) %>%
  calculate_ab_effect(., ab_effectiveness = 0.9) %>%
  simulate_contingency_table() %>%
  .[0,]
simulated_ab_effects <-
  matrix(NA,
       ncol = ncol(simulated_ab_effects),
       nrow = n_simulations) %>%
  `colnames<-`(colnames(simulated_ab_effects)) %>%
  as.data.frame()

pb <- utils::txtProgressBar(min = 0, max = n_simulations, style = 3)
for (i in 1:n_simulations) {
  simulated_ab_effects[i, ] <-
    calculate_contingency_table(n_total, prop_need,
                                ab_choice_sensitiv, ab_choice_spec,
                                rounding = FALSE) %>%
    calculate_ab_effect(., ab_effectiveness = ab_eff) %>%
    simulate_contingency_table()
  utils::setTxtProgressBar(pb, i)
}
close(pb)

# Plot histograms, one per column
simulated_ab_effects %>%
  tidyr::pivot_longer(cols = everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "darkblue") +
  facet_grid(rows = vars(name), scales = "fixed") +
  theme_minimal()

####################
# Calculate correct identification and false classifications for expert team
# Expert team aims to find the *SPURIOUS TREATED CASES*

simulated_ab_effects <- simulated_ab_effects %>%
  mutate(
    corr_ident_spur_exp = abpoz_noinf_fp * expert_sens,
    fals_ident_spur_exp = abpoz_noinf_fp - corr_ident_spur_exp,
    corr_ident_gtrt_exp = abpoz_noinf_tp * expert_spec,
    fals_ident_gtrt_exp = abpoz_noinf_tp - corr_ident_gtrt_exp
  )

dat_expert <- simulated_ab_effects %>%
  select(
    corr_ident_spur_exp, fals_ident_spur_exp,
    corr_ident_gtrt_exp, fals_ident_gtrt_exp
  )

pb <- utils::txtProgressBar(min = 0, max = n_simulations, style = 3)
for (i in 1:n_simulations) {
  dat_expert[i,] <-
    dat_expert[i,] %>%
    simulate_contingency_table() %>%
    .[,1:4]
  utils::setTxtProgressBar(pb, i)
}
close(pb)

colnames(dat_expert) <-
  c("corr_ident_spur_sim", "fals_ident_spur_sim",
    "corr_ident_gtrt_sim", "fals_ident_gtrt_sim")

simulated_ab_effects <- bind_cols(simulated_ab_effects, dat_expert)

simulated_ab_effects %>%
  .[,c((ncol(simulated_ab_effects)-3):ncol(simulated_ab_effects))] %>%
  tidyr::pivot_longer(cols = everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "darkblue") +
  facet_grid(rows = vars(name), scales = "fixed") +
  theme_minimal()

