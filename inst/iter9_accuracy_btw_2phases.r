library(dplyr)
library(ggplot2)
library(splines)


sim_study <- function(x, y, gain, base_acc = 108/141){

  if( gain + base_acc > 1){
    stop("Gain + 0.77 must be less than 1")
  }

  res_good <- c( rbinom(1, x, base_acc),  rbinom(1, y, base_acc + gain))
  res_bad <- c( x - res_good[1], y - res_good[2])

  suppressWarnings(
    res <- prop.test(res_good, c(x, y))
  )

  return(res$p.value)
}
Sim_study <- Vectorize(sim_study)



# std. therapy accuracy: 33/141 (~23.4%)
# absolute accuracy:     40/141 (~28.4%)
# optimistic gain: until 13/141 (~9.2%)
# absolute threshold accuracy: 101/141 ~ 71.63%


x <- 100
kappa <- 1
y <- round(x * kappa)
gain <- 0.15
n_sim <- 1000

obs_powa <-
  Sim_study(rep(x, n_sim), rep(y, n_sim), rep(gain, n_sim)) %>%
  {sum(ifelse(. < 0.05, 1, 0))} %>% { . / n_sim}


dt_sim <- expand.grid(sample_size = seq(100, 400, 50),
                      kappa = c(0.5,1,1.5,2,2.5),
                      gain = seq(0.10, 0.20, 0.025)
                      #base_acc = c(101/141,108/141)
                      ) %>%
  mutate(x = floor(sample_size / (1 + kappa)),
         y = sample_size - x,
         obs_powa = NA,
         sample_size = x + y)

# Initialize the success and obs_powa columns
dt_sim <- dt_sim %>% mutate(success = NA_integer_, obs_powa = NA_real_)

pb <- txtProgressBar(min = 0, max = nrow(dt_sim), style = 3)
for (i in 1:nrow(dt_sim)){
  # Simulate and count the number of times the p-value is less than 0.05
  dt_sim$success[i] <- Sim_study(rep(dt_sim$x[i], n_sim),
                                 rep(dt_sim$y[i], n_sim),
                                 rep(dt_sim$gain[i], n_sim)) %>%
    {sum(ifelse(. < 0.05, 1, 0), na.rm = TRUE)}

  # Calculate the observed power
  dt_sim$obs_powa[i] <- dt_sim$success[i] / n_sim

  setTxtProgressBar(pb, i)
}
close(pb)




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
            ns(kappa, 3) + ns(gain, 3) + ns(sample_size,3)
           + ns(gain, 4) * ns(sample_size,2),
           family = binomial(link = "logit")
           , data = dt_sim)

mod %>% effects::predictorEffects() %>% plot()

DHARMa::plotResiduals(mod)


pr <- expand.grid(sample_size = seq(100, 400, 5),
                   kappa = c(0.5,1,1.5,2,2.5),
                   gain = seq(0.10, 0.20, 0.01)) %>%
  mutate(x = floor(sample_size / (1 + kappa)),
         y = sample_size - x,
         success = NA_integer_, obs_powa = NA_real_)

pr$pr <- predict(mod, type = "response", newdata = pr)

pr %>%
  ggplot(aes(x = sample_size, y = pr,
             color = kappa,
             group = kappa)) +
  geom_line() +
  theme_minimal() +
  geom_hline(yintercept = c(0.8,.9), linetype = "dashed", color = "salmon4") +
  labs(title = "Power of the test",
       x = "Sample Size",
       y = "Predicted Power",
       color = "Kappa") +
  theme(legend.position = "bottom") +
  facet_wrap(~gain)

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


dt_sim <- expand.grid(sample_size = seq(100, 400, 50),
                      kappa = seq(0.25,0.5,length.out=6),
                      gain = seq(0.10, 0.17, 0.01)
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
             (ns(kappa, 2) + ns(gain, 2) + ns(sample_size,3))^3,
           family = binomial(link = "logit")
           , data = dt_sim)

#mod %>% effects::predictorEffects() %>% plot()

DHARMa::plotResiduals(mod)

pr <- expand.grid(sample_size = seq(150, 200, 2),
                  kappa = c(0.25,0.33,0.5),
                  gain = seq(0.12, 0.15, 0.01)) %>%
  mutate(x = floor(sample_size * kappa),
         y = sample_size - x,
         success = NA_integer_, obs_powa = NA_real_)

pr$pr <- predict(mod, type = "response", newdata = pr)

pr %>%
  ggplot(aes(x = sample_size, y = pr,
             color = kappa,
             group = kappa)) +
  geom_line() +
  theme_minimal() +
  geom_hline(yintercept = c(0.8,.9), linetype = "dashed", color = "salmon4") +
  labs(title = "Power of the test",
       x = "Sample Size",
       y = "Predicted Power",
       color = "Kappa") +
  theme(legend.position = "bottom") +
  facet_wrap(~gain)

