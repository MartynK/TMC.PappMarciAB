library(dplyr)
library(ggplot2)
library(splines)


ADD_MORE_SIMULATIONS <- FALSE

n_sim <- 3000 # FRAGILE, needed for glm

if (ADD_MORE_SIMULATIONS) {


  ###########
  # second iter

  sim_study2 <- function(x, y, gain, base_acc = 101/141){

    if( gain + base_acc > 1){
      stop("Gain + base_acc must be less than 1")
    }

    res_good <- c( rbinom(1, x+y, base_acc),  rbinom(1, y, base_acc + gain))
    res_bad <- c( x + y - res_good[1], y - res_good[2])

    suppressWarnings(
      res <- prop.test(res_good, c(x+y, y))
    )

    return(res$p.value)
  }
  Sim_study2 <- Vectorize(sim_study2)


  dt_sim <- expand.grid(sample_size = seq(100, 200, 5),
                        kappa = c(seq(0.25,0.5,length.out=6),0.33),
                        gain = seq(0.18, 0.20, 0.01)
                        #base_acc = c(101/141,108/141)
  ) %>%
    mutate(x = floor(sample_size * kappa),
           y = sample_size - x,
           obs_powa = NA)

  # Initialize the success and obs_powa columns
  dt_sim <- dt_sim %>% mutate(success = NA_integer_, obs_powa = NA_real_)

  pb <- txtProgressBar(min = 0, max = nrow(dt_sim), style = 3)
  for (i in 1:nrow(dt_sim)){
    # Simulate and count the number of times the p-value is less than 0.05
    dt_sim$success[i] <- Sim_study2(rep(dt_sim$x[i], n_sim),
                                    rep(dt_sim$y[i], n_sim),
                                    rep(dt_sim$gain[i], n_sim)) %>%
      {sum(ifelse(. < 0.05, 1, 0), na.rm = TRUE)}

    # Calculate the observed power
    dt_sim$obs_powa[i] <- dt_sim$success[i] / n_sim

    setTxtProgressBar(pb, i)
  }
  close(pb)

  dt_sim_bu <- dt_sim

  attempt <-
    try({
      load(here::here("data","iter10_accuracy_btw_2phases_abs.rds"))
    })

  if (!inherits(attempt, "try-error")) {
    dt_sim <- bind_rows(dt_sim, dt_sim_bu) # error if _bu not avail. & no 'new sims'
  }

  saveRDS(dt_sim, here::here("data","iter10_accuracy_btw_2phases_abs.rds"))
}


dt_sim <- readRDS(here::here("data","iter10_accuracy_btw_2phases_abs.rds"))

fig_1 <-
  dt_sim %>%
    ggplot(aes(x = sample_size, y = obs_powa,
               color = kappa,
               group = interaction(gain, kappa))) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    #scale_color_brewer(palette = "Set1") +
    labs(title = "Power of the test",
         x = "Gain",
         y = "Observed Power",
         color = "Kappa") +
    theme(legend.position = "bottom") +
    geom_hline(yintercept = c(0.8,.9), linetype = "dashed", color = "salmon4") +
    facet_wrap(~gain)

mod <- glm(cbind(success, n_sim - success) ~
             (ns(kappa, 3) + ns(gain, 2) + ns(sample_size,3))^2
           ,
           family = binomial(link = "logit")
           , data = dt_sim)

#mod %>% effects::predictorEffects() %>% plot()

DHARMa::plotResiduals(mod)

pr <- expand.grid(sample_size = seq(100, 200, 2),
                  kappa = c(0.25,0.33,.4,.5),
                  gain = seq(0.17, 0.20, 0.01)) %>%
  mutate(x = floor(sample_size * kappa),
         y = sample_size - x,
         success = NA_integer_, obs_powa = NA_real_) %>%
  filter(x >= 60)

pr$pr <- predict(mod, type = "response", newdata = pr)

fig_2 <-
  pr %>%
    filter(gain == 0.19) %>%
    ggplot(aes(x = sample_size, y = pr,
               color = kappa,
               group = kappa)) +
    geom_line() +
    theme_minimal() +
    geom_hline(yintercept = c(0.8,.9), linetype = "dashed", color = "salmon4") +
    labs(title = "Power of the study, 1st phase >= 60 subjects",
         x = "Sample Size",
         y = "Power",
         color = "Ratio of samp.size in Phase1/Phase2") +
    theme(legend.position = "bottom")


fig_3 <-
  pr %>%
    ggplot(aes(x = sample_size, y = pr,
               color = kappa,
               group = kappa)) +
    geom_line() +
    theme_minimal() +
    geom_hline(yintercept = c(0.8,.9), linetype = "dashed", color = "salmon4") +
    labs(title = "Power of the test faceted by gain in accuracy (%)",
         x = "Sample Size",
         y = "Power",
         color = "Ratio of samp.size in Phase1/Phase2") +
    theme(legend.position = "bottom") +
    facet_wrap(~gain)

