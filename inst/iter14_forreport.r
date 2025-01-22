source(here::here("inst","functions","load_stuff.r"))
#load(here::here("data","dat_full.rds"))

# Direct load of the excel compiled by me
dat_full <- readxl::read_xlsx(here::here("inst","extdata",
                                         "2024.12.21","t1t0.xlsx")) %>%
  rename( t_min_1 = `T-1`,
          inf = status) %>%
  mutate( inf = as.factor(inf) %>% forcats::fct_recode( "0" = "no_infection" ,
                                                        "1" = "infection" )
          )

fig_1 <-
  dat_full %>%
  # recode inf if 1: infection if 0:no infection
  mutate( inf = ifelse(inf ==1 , "infection", "no infection")) %>%
    ggplot(.,aes(x = diff, group = inf)) +
    # histogram with density
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgrey", color = "black") +
    # kernel smooth
    geom_density(alpha = 0.5, fill = "blue") +
    theme_minimal() +
    facet_wrap(~inf) +
    scale_x_log10() +
    labs(x = "Difference between T0 and T-1",
         y = "Density (for histogram)")

fig_2 <-
  dat_full %>%
  # recode inf if 1: infection if 0:no infection
  mutate( inf = ifelse(inf ==1 , "infection", "no infection")) %>%
  ggplot(.,aes(x = ratio_over_diff, group = inf)) +
  # histogram with density
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgrey", color = "black") +
  # kernel smooth
  geom_density(alpha = 0.5, fill = "blue") +
  theme_minimal() +
  facet_wrap(~inf) +
  scale_x_log10() +
  labs(x = "(T0 / T-1)/(T-1 - T0)",
       y = "Density (for histogram)")


# Calculate a bunch of new variables
dat_full <- dat_full %>%
  mutate(
         pct_t0_log = log(t0),
         val = ratio_over_diff
         )

# ROC anal. wth the 'val' (easily changable) predictor
roc_curve <- roc(inf ~ val, data = dat_full, direction = ">", levels = c(0, 1))

# ROC görbe rajzolása
# Add confidence intervals as a shaded area
ci <- ci.se(roc_curve, specificities = seq(0, 1, by = 0.01))  # Specificity range
## FIG 3 !!!
# plot(ci, type = "shape", col = rgb(0.2, 0.5, 0.8, 0.3))  # Add shaded confidence interval

# # Print AUC
auc_roc_curve <- pROC::auc(roc_curve)
# print(auc(roc_curve))
# (cutoff)

# Define a cutoff percentile for the simple ROC analysis (reproducing orig. findings)
CUTOFF_PERC <- 0.1
cutoff <- quantile(dat_full$val, CUTOFF_PERC, na.rm = TRUE)

dat_full <- dat_full %>%
  mutate(pred = ifelse(val > cutoff,1,0))

# Print confusion table (NPV=0.38)
tab_1 <-
  confusionMatrix(
    factor(dat_full$pred),
    factor(dat_full$inf),
    positive = "1")


#colnames(dat_full)

# Define the predictors to use
predictors <- c(
   "t_min_1" ,        "t0",
   "diff"    ,        "ratio",
   "ratio_over_diff",
   "pct_t0_log"
                )

# Replace any NA values for simplicity
dat_clean <- dat_full %>%
  select(inf, all_of(predictors)) %>%
  # for all cols other than inf, impute missings by mean of column
  mutate( across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  filter(inf == round(inf)) %>%
  na.omit() %>%
  mutate(inf = as.factor(inf) %>% forcats::fct_recode("No_infection" = "2",
                                                      "Infection" = "1"))


# Set control parameters for rpart
control_params <- rpart.control(maxdepth = 3, cp = 0, minsplit = 10)

# Define the loss matrix
loss_matrix <- matrix(
  c(0, 1,   # Cost of predicting 'Appropriate' when true class is 'Appropriate': 0
    # Cost of predicting 'Not_appropriate' when true class is 'Appropriate': 5
    5, 0    # Cost of predicting 'Appropriate' when true class is 'Not_appropriate': 0
    # Cost of predicting 'Not_appropriate' when true class is 'Not_appropriate': 0
  ),
  nrow = 2,
  byrow = TRUE
)

# Set row and column names to match the factor levels of 'inf'
rownames(loss_matrix) <- colnames(loss_matrix) <- c("Infection", "No_infection")

# Custom summary function to calculate PPV
ppvSummary <- function(data, lev = NULL, model = NULL) {

  THRESHOLD <- 0.8
  data <- data %>%
    mutate(man_pred = ifelse(No_infection >= THRESHOLD,0,1))

  if (sum(data$man_pred) == nrow(data)) {
    return(c(NPV = 0))
  }

  res <- 1

  try(silent=TRUE, {
    tab <- data %>%
      select(obs, man_pred) %>% table()

    res <- tab %>% {.[2,1]/sum(.[,1])}
  })

  c(NPV = res)
}

# Set up cross-validation control
train_control <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = ppvSummary,
  classProbs = TRUE,
  savePredictions = TRUE,
  verboseIter = TRUE,
  ## 'mote' sampling biases the model towards the minority class (lowers NPV!!!)
  #sampling = "smote" # this is where the sampling DMwR goes
)

# Define the grid of hyperparameters
tune_grid <- expand.grid(
  cp = seq(0.0, 0.05, length.out = 11),
  maxdepth = 1:4,
  minsplit = seq(20, 40, by = 5)
)

######
# RANDOM FOREST

# Define the grid of 'mtry' values to try
tune_grid <- expand.grid(
  mtry = seq(2, (ncol(dat_clean) - 1), by = 1)  # Adjust based on your data
)

set.seed(123)  # For reproducibility

# Fitting a "big forest" for varimp
rf_model <- train(
  inf ~ .,
  data = dat_clean,
  method = "rf",
  metric = "NPV",                # Optimize for PPV
  trControl = train_control,
  tuneGrid = tune_grid,
  ntree = 1000                    # Number of trees in the forest
)




varImp_rf <- varImp(rf_model, scale = FALSE)

# Predictions on training data
train_preds <- predict(rf_model, dat_clean)

# Confusion matrix
conf_matrix <- confusionMatrix(train_preds, dat_clean$inf, positive = "No_infection")


# # Eval part
# print(rf_model)
# plot(rf_model)
#
# print(varImp_rf)
# plot(varImp_rf)
#
# print(conf_matrix)


save.image(here::here("data","end_state_iter14_forreport.rdata"))
