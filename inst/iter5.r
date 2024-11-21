

# Function to calculate the contingency table
calculate_contingency_table <- function(n_total = 100,
                                        prop_need = 0.75,
                                        prop_neg = 0.2,
                                        NPV = 0.9) {

  validate <- TRUE

  # Total number of positives and negatives in the population
  P <- n_total * prop_need        # Total actual positives
  N <- n_total - P                # Total actual negatives

  # Total number of negative test results
  total_neg_tests <- n_total * prop_neg

  NPV_corr <- NPV
  if (((1-prop_need) * n_total) / total_neg_tests < NPV) {
    #stop("The expected number of negative tests is greater than the number of actual negatives")
    NPV_corr <- (1-prop_need) / prop_neg # override NPV to max. possible
    validate <- FALSE
  }

  # Calculate True Negatives (TN) and False Negatives (FN)
  TN <- NPV_corr * total_neg_tests
  FN <- total_neg_tests - TN

  # Calculate True Positives (TP) and False Positives (FP)
  TP <- P - FN
  FP <- n_total - TP - TN - FN

  # Return the contingency table elements
  list(tp = TP, fp = FP, tn = TN, fn = FN, NPV = NPV_corr, validate = validate)
}

# Function to simulate a contingency table with error
simulate_contingency_table <- function(expected_tbl, frac_tst = 1) {

  # get the table values, dis the negative values
  tab_val <- expected_tbl %>% unlist %>% pmax(0)

  # Total number of observations in the table
  n_total <- sum(tab_val[1:4])

  # Convert the expected counts into probabilities
  cutoffs <-  {tab_val / n_total} %>% {c(.[1],
                                         .[1] + .[2],
                                         .[1] + .[2] + .[3],
                                         .[1] + .[2] + .[3] + .[4]
  )}

  # Simulate counts using multinomial distribution
  pts <- c(0,0,0,0)
  n_act <- 0
  while (n_act < n_total) {
    # random seed for situation
    rand <- runif(1)
    # Testing if positive if gonna be evaled
    if (rand <= cutoffs[2]) {
      if ( runif(1) > frac_tst) {
        next()
      }
    }

    if (rand <= cutoffs[1]) {
      pts[1] <- pts[1] + 1
    } else if (rand <= cutoffs[2]) {
      pts[2] <- pts[2] + 1
    } else if (rand <= cutoffs[3]) {
      pts[3] <- pts[3] + 1
    } else {
      pts[4] <- pts[4] + 1
    }
    n_act <- n_act + 1

  }


  # Convert back to named list
  simulated_tbl <- data.frame(
    tp = pts[1],
    fp = pts[2],
    tn = pts[3],
    fn = pts[4]
  )

  return(simulated_tbl)
}


NSIM <- 200

dat_design_mat <-
  expand.grid(
    n_total = seq(50, 200, length.out = 2),
    prop_neg_a = seq(0.15, 0.35, length.out = 5),
    NPV_a = seq(0.5, 0.9, length.out = 5),

    prop_neg_b = seq(0.35, 0.60, length.out = 3),
    NPV_b = seq(0.2, 0.9, length.out = 4),
    frac_tst = union(seq(0.1, 0.5, length.out = 3),1)
  ) %>%
  mutate(
    tp_a = NA,
    fp_a = NA,
    tn_a = NA,
    fn_a = NA,
    tp_b = NA,
    fp_b = NA,
    tn_b = NA,
    fn_b = NA,
    NPV_corr_a = NA,
    NPV_corr_b = NA
  )


for (i in 1:nrow(dat_design_mat)) {
  act_a <- calculate_contingency_table(n_total = dat_design_mat$n_total[i],
                                       prop_neg = dat_design_mat$prop_neg_a[i],
                                       NPV = dat_design_mat$NPV_a[i])
  dat_design_mat$tp_a[i] <- act_a$tp
  dat_design_mat$fp_a[i] <- act_a$fp
  dat_design_mat$tn_a[i] <- act_a$tn
  dat_design_mat$fn_a[i] <- act_a$fn
  dat_design_mat$NPV_corr_a[i] <- act_a$NPV

  act_b <- calculate_contingency_table(n_total = dat_design_mat$n_total[i],
                                       prop_neg = dat_design_mat$prop_neg_b[i],
                                       NPV = dat_design_mat$NPV_b[i])
  dat_design_mat$tp_b[i] <- act_b$tp
  dat_design_mat$fp_b[i] <- act_b$fp
  dat_design_mat$tn_b[i] <- act_b$tn
  dat_design_mat$fn_b[i] <- act_b$fn
  dat_design_mat$NPV_corr_b[i] <- act_b$NPV

}


dat_theta <- cbind(
  calculate_contingency_table(n_total = n_arm, prop_neg = .25, NPV=.75) %>% as.data.frame(),
  calculate_contingency_table(n_total = n_arm, prop_neg = .50, NPV=.4) %>% as.data.frame()
)

dat_res <- dat_theta %>%
  janitor::clean_names() %>%
  slice(rep(1, each=NSIM)) %>%
  mutate( prop_test_p = NA)

for (i in 1:NSIM) {

  dat_res[i,1:4]  <- simulate_contingency_table(dat_theta[1,1:4], frac_tst =.25)
  dat_res[i,7:10] <- simulate_contingency_table(dat_theta[1,7:10])

  try({
    test <- prop.test(
      c(dat_res$tn[i],dat_res$tn_2[i]),
      c(dat_res$tn[i]+dat_res$fn[i],dat_res$tn_2[i]+dat_res$fn_2[i]),
    )

    dat_res$prop_test_p[i] <- test$p.value
  })

}

hist(dat_res$prop_test_p,breaks = 20)

sum(dat_res$prop_test_p < 0.05, na.rm = TRUE) / NSIM

