library(ggplot2)
library(splines)
library(dplyr)

load(here::here("data","dat_res_all.rds"))

get_total_n <- function(n_total, frac_tst, tp_a, fp_a, prop_neg_a,
                        evaled = FALSE,
                        dropout = .1) {

  if (is.na(tp_a[1]) | is.na(fp_a[1])) {
    tp_a <- (1 - prop_neg_a) * n_total/2
    fp_a <- (1 - prop_neg_a) * n_total/2
  }

  if (evaled == FALSE) {
    res <- (n_total + (tp_a + fp_a) / (frac_tst))
  } else {
    res <- (n_total + (tp_a + fp_a) )
  }

  res <- res %>%
    {./(1-dropout)} %>%
    ceiling() %>% {./2} %>% ceiling() %>% {.*2}

  return(res)
}


dat_res_all <- dat_res_all %>%
  mutate(success = ifelse(prop_test_p < 0.05, 1, 0),
         n_total_corr =  get_total_n(n_total, frac_tst, tp_a, fp_a, prop_neg_a))


mod <- glm(success ~ (frac_tst
                      + ns( n_total_corr, df = 2)
                      + ns( prop_neg_a, df = 2)
                      + ns( npv_corr_a, df = 2)
                      + ns( prop_neg_b, df = 2)
                      + ns( npv_corr_b, df = 2)
                      )^2,
           data = dat_res_all,
           family = "binomial")

save(mod, file = here::here("data","mod.rds"))

mod %>% summary()
#mod %>% effects::predictorEffects() %>% plot()

##Takes ~1min for 1.2Mn sim.
#DHARMa::plotResiduals(mod)



dat_res_all$pred <- predict(mod, type = "response", newdata = dat_res_all)

# dat_res_all %>%
#   ggplot(aes(x = n_total_corr, y = pred,
#              group = paste(npv_a,npv_b,prop_neg_a,prop_neg_b),
#
#              alpha = npv_b)) +
#   geom_point(aes(color = prop_neg_a)) +
#   geom_line() +
#   theme_minimal() +
#   scale_y_continuous(limits = c(0.7,1)) +
#   facet_grid(rows = vars(prop_neg_b), cols = vars(frac_tst))

dat_res_all %>%
  select(n_total_corr, npv_corr_b, npv_b, prop_neg_b, prop_neg_a, frac_tst) %>%
  group_by(n_total_corr, npv_corr_b, npv_b, prop_neg_b, prop_neg_a, frac_tst) %>%
  slice(1) %>%
  ggplot(aes(x = npv_b
             , y = npv_corr_b
             , color = prop_neg_b
             )) +
  geom_point(alpha = .01) +
  geom_abline( slope = 1, intercept = 0, color = "salmon4") +
  theme_minimal()



# dat_res_all %>%
#   filter(prop_neg_a == 0.25,
#          prop_neg_b == 0.5,
#          frac_tst == 1
#          ,npv_a == 0.85
#          ) %>%
#   # Calculate mproprtion of successes for each category of a combination of factors
#   group_by(n_total_corr, npv_corr_b) %>%
#   mutate(prop_success = sum(success)/n()) %>%
#   ggplot(aes(x = n_total_corr, y = pred,
#              group = paste(npv_a,npv_b)
#              ,color = npv_corr_b
#              #,size = npv_a
#              #,alpha = npv_b
#              )) +
#   #geom_point() +
#   geom_point(aes(y=prop_success),shape=3,size=2.5) +
#   geom_line() +
#   theme_minimal() +
#   scale_y_continuous(limits = c(0.7,1))

###########

pr <- expand.grid(
  n_total = seq(50, 150, length.out = 30),
  prop_neg_a = 0.25,
  npv_corr_a = seq(0.85, 0.95, length.out = 1),
  prop_neg_b = 0.5,
  npv_corr_b = seq(0.3, 0.5, length.out = 3),
  frac_tst = c(0.5,1)  #c(.33,.5,1)
) %>%
  mutate(
         n_total_corr = get_total_n(n_total, frac_tst, tp_a = NA, fp_a = NA,
                                    prop_neg_a = prop_neg_a),
         n_total_eval = get_total_n(n_total, frac_tst, tp_a = NA, fp_a = NA,
                                    prop_neg_a = prop_neg_a, evaled = TRUE))

pr$pr <- predict(mod, type = "response", newdata = pr)

pr %>%
  ggplot(aes(x = n_total_eval, y = pr
             ,color = npv_corr_b
             ,group = npv_corr_b
             #,size = npv_a
             #,alpha = npv_b
  )) +
  #geom_point() +
  #geom_point(aes(y=prop_success),shape=3,size=2.5,data = dat_res_all) +
  geom_line() +
  theme_minimal() +
  geom_hline(yintercept = .8, color = "salmon4", linetype = "dashed" ) +
  geom_hline(yintercept = .9, color = "salmon4", linetype = "dashed" ) +
  scale_y_continuous(limits = c(0.7,1)) +
  scale_x_continuous(limits = c(50,300)) +
  facet_grid(rows = vars(frac_tst),
             labeller = labeller(frac_tst = c(`0.5` = "Test Fraction: 0.5",
                                                `1` = "Test Fraction: 1"))) +
  labs( y = "Power",
        color = "Arm B NPV")

