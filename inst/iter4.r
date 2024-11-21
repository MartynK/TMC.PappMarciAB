# This iteration can simulate n trials about an arm with error
# Returns two values for abpoz_noinf category and the 'detection' of them is
# an as of yet open question

# Load necessary libraries
library(dplyr)
library(emmeans)
library(flextable)

# Necessary parameters
##n_simulations <- 1000
n_total            <- 100
prop_need          <- 0.75
ab_choice_sensitiv <- 0.6
ab_choice_spec     <- 0.6

ab_eff <- 0.9

# Expert team aims to find the *SPURIOUS TREATED CASES*
expert_sens <- 0.9
expert_spec <- 0.8

set.seed(12)

# Function to calculate the contingency table
calculate_contingency_table <- function(n_total = n_total,
                                        prop_need = 0.5,
                                        ab_choice_sensitiv = 0.75,
                                        ab_choice_spec = 0.875) {
  p <- n_total * prop_need
  n <- n_total - p
  tp <- ab_choice_sensitiv * p
  fn <- p - tp
  tn <- ab_choice_spec * n
  fp <- n - tn


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

fun_simulate_arm <- function(n_total = 100,
                             prop_need = 0.5,
                             ab_choice_sensitiv = 0.75,
                             ab_choice_spec = 0.875,
                             ab_effectiveness = 0.9,
                             expert_sens = 0.8,
                             expert_spec = 0.7,
                             n_simulations = 1000) {
  # Simulate an AB effect table
  simulated_ab_effects <-
    calculate_contingency_table(n_total, prop_need,
                                ab_choice_sensitiv = ab_choice_sensitiv,
                                ab_choice_spec = ab_choice_spec) %>%
    calculate_ab_effect(., ab_effectiveness = ab_eff) %>%
    simulate_contingency_table()  %>%
    ####################
    # Calculate correct identification and false classifications for expert team
    # Expert team aims to find the *SPURIOUS TREATED CASES*
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
    ) %>%
    simulate_contingency_table() %>%
    .[,1:4] %>%
    `colnames<-`(c("corr_ident_spur_sim", "fals_ident_spur_sim",
                   "corr_ident_gtrt_sim", "fals_ident_gtrt_sim"))

  simulated_ab_effects <- bind_cols(simulated_ab_effects, dat_expert)
  return(simulated_ab_effects)
}

dat_study <-
  bind_rows(
    fun_simulate_arm(n_total = n_total,
                     prop_need = prop_need,
                     # If AB was not given, prop. patients who truly did not needed it
                     ab_choice_sensitiv = 0.9,
                     # If AB was given, prop. patients who truly needed it
                     ab_choice_spec     = 0.6) %>%
      mutate(arm = "A"),
    fun_simulate_arm(n_total = n_total,
                     prop_need = prop_need,
                     ab_choice_sensitiv = 0.6,
                     ab_choice_spec     = 0.9) %>%
      mutate(arm = "B")
  )

# study ab negative proportion difference
dat_long_study <-
  bind_rows(
    data.frame(
      inf = c(
        rep(TRUE, dat_study$abneg_yesinf[1]),
        rep(FALSE, dat_study$abneg_noinf[1])
      )
    ) %>% mutate(arm = "A"),
    data.frame(
      inf = c(
        rep(TRUE, dat_study$abneg_yesinf[2]),
        rep(FALSE, dat_study$abneg_noinf[2])
      )
    ) %>% mutate(arm = "B")
  )


mod <- glm(inf ~ arm, data = dat_long_study, family = binomial)

# Compute estimated marginal means (EMMs)
emm <- emmeans(mod, ~ arm, type = "response")

# Regrid to the response (probability) scale
emm_resp <- regrid(emm, transform = "response")

# Compute contrasts (differences) on the probability scale
contrast_results <- contrast(emm_resp, method = "pairwise", adjust = "none")

# Display the results with 95% confidence intervals
summary(contrast_results, infer = c(TRUE, FALSE))


#####
#


# study ab negative proportion difference
dat_long_study_abpoz <-
  bind_rows(
    data.frame(
      inf = c(
        rep(TRUE, dat_study$abpoz_noinf_tp[1] +
                  dat_study$abpoz_noinf_fp[1] +
                  dat_study$abpoz_yesinf[1]),
        rep(FALSE, n_total -  (dat_study$abpoz_noinf_tp[1] +
                              dat_study$abpoz_noinf_fp[1] +
                              dat_study$abpoz_yesinf[1]))
      )
    ) %>% mutate(arm = "A"),
    data.frame(
      inf =  c(
        rep(TRUE, dat_study$abpoz_noinf_tp[2] +
              dat_study$abpoz_noinf_fp[2] +
              dat_study$abpoz_yesinf[2]),
        rep(FALSE, n_total -  (dat_study$abpoz_noinf_tp[2] +
              dat_study$abpoz_noinf_fp[2] +
              dat_study$abpoz_yesinf[2]))
      )
    ) %>% mutate(arm = "B")
  )

mod_abpoz <- glm(inf ~ arm, data = dat_long_study_abpoz, family = binomial)

# Compute estimated marginal means (EMMs)
emm <- emmeans(mod_abpoz, ~ arm, type = "response")

# Regrid to the response (probability) scale
emm_resp <- regrid(emm, transform = "response")

# Compute contrasts (differences) on the probability scale
contrast_results <- contrast(emm_resp, method = "pairwise", adjust = "none")

# Display the results with 95% confidence intervals
summary(contrast_results, infer = c(TRUE, FALSE))



fun_generate_cont_table <- function(tp, fp, tn, fn) {

  # validate that there are 4 nonnegative numbers inputted
  fun_check_num_nonneg <- function(num) {
    num %>% as.numeric %>% {.>=0} %>% ifelse(is.na(.), FALSE, .)
  }
  Fun_check_num_nonneg <- Vectorize(fun_check_num_nonneg)
  Fun_check_num_nonneg(c(tp, fp, tn, fn)) %>% all %>% stopifnot()

  # Create a more intuitive color scale function for a numeric vector
  get_colors <- function(values) {
    colors <- colorRampPalette(c("#D3D3D3", "#4D4D4D"))(100)
    minv <- min(values, na.rm=TRUE)
    rangeval <- max(values, na.rm=TRUE) - minv
    cols <- colors[1 + as.integer(99 * ((values - minv) / rangeval))]
    return(cols)
  }

  colors. <- get_colors(c(tp, fp, tn, fn))

  # Apply the color scale to the table (assign colors per cell) and add row names
  tbl_cross_humread <-
    {c(tp, fn, fp, tn)} %>%
    matrix(., ncol=2,byrow=TRUE) %>%
    as.data.frame() %>%
    `colnames<-`(c("poz_pred", "neg_pred")) %>%
    `rownames<-`(c("poz_cond", "neg_cond")) %>%
    tibble::rownames_to_column(" ") %>%
    flextable() %>%
    flextable::bg(i = 1, j = 2, bg = colors.[1]) %>%
    flextable::bg(i = 1, j = 3, bg = colors.[4]) %>%
    flextable::bg(i = 2, j = 2, bg = colors.[2]) %>%
    flextable::bg(i = 2, j = 3, bg = colors.[3])


  # # Display the table
  # tbl_cross_humread

  return(list(
    tbl_cross = tbl_cross_humread
  ))

}

fun_generate_cont_table(tp = dat_study$abpoz_noinf_tp[1] + dat_study$abpoz_yesinf[1],
                        fp = dat_study$abpoz_noinf_fp[1],
                        tn = dat_study$abneg_noinf[1],
                        fn = dat_study$abneg_yesinf[1])$tbl_cross


fun_generate_cont_table(tp = dat_study$abpoz_noinf_tp[2] + dat_study$abpoz_yesinf[2],
                        fp = dat_study$abpoz_noinf_fp[2],
                        tn = dat_study$abneg_noinf[2],
                        fn = dat_study$abneg_yesinf[2])$tbl_cross



fun_generate_cont_table(tp = dat_study$abpoz_yesinf[1],
                        fp = dat_study$abpoz_noinf_tp[1] + dat_study$abpoz_noinf_fp[1],
                        tn = dat_study$abneg_noinf[1],
                        fn = dat_study$abneg_yesinf[1])$tbl_cross


fun_generate_cont_table(tp = dat_study$abpoz_yesinf[2],
                        fp = dat_study$abpoz_noinf_tp[2] + dat_study$abpoz_noinf_fp[2],
                        tn = dat_study$abneg_noinf[2],
                        fn = dat_study$abneg_yesinf[2])$tbl_cross
