library(ggplot2)
library(dplyr)
library(pROC)
library(randomForest)
library(caret)

load(here::here("data","dat_full.rds"))

dat_full <-
  dat_full %>%
    mutate(rel_diff = baseline_pct - pct_t0,
           rel_rat  = pct_t0 / baseline_pct,
           inf = ifelse( dataset == "Infection", 1, 0)) %>%
  filter( dataset %in% c("Infection","No infection")) %>% filter(rel_rat != 1) # striiiict filter


# The dataset shrinks from209 to 63 with 10 noinf.cases
with(dat_full, table( inf, rel_diff == 0))



fig_1 <-
  dat_full %>%
    ggplot(.,aes(x = pct_t0, group = dataset)) +
    # histogram with density
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgrey", color = "black") +
    # kernel smooth
    geom_density(alpha = 0.5, fill = "blue") +
    theme_minimal() +
    facet_wrap(~dataset) +
    scale_x_log10()

fig_2 <-
  dat_full %>%
    mutate(pct_relevant_diff = baseline_pct - pct_t0,
           pct_relevant_diff_win = ifelse(pct_relevant_diff > 100,100, pct_relevant_diff )) %>%
  # Calculate the difference
    ggplot(.,aes(x = sqrt(pct_relevant_diff_win) , group = dataset)) +
    # histogram with density
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgrey", color = "black") +
    # kernel smooth
    geom_density(alpha = 0.5, fill = "blue") +
    theme_minimal() +
    facet_wrap(~dataset)

fig_3 <-
  dat_full %>%
  ggplot(.,aes(x = pct_delta_0_24 , group = dataset)) +
  # histogram with density
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgrey", color = "black") +
  # kernel smooth
  geom_density(alpha = 0.5, fill = "blue") +
  theme_minimal() +
  facet_wrap(~dataset) +
  scale_x_log10()



# Calculate a bunch of new variables
dat_full <- dat_full %>%
  mutate(
         pct_t0_log = log(pct_t0),
         pct_t8_log = log(pct_t8),
         pct_delta_0_8_hand = pct_t8 - pct_t0,
         pct_delta_0_8_log= pct_t8_log - pct_t0_log,
         pct_delta_0_8_perc = pct_t8 / pct_t0,
         cumsum_16 = pct_t0 + pct_t8 + pct_t16,
         cumsum_8 = pct_t0 + pct_t8,
         cumprod_16 = pct_t0 * pct_t8 * pct_t16,
         cumprod_8 = pct_t0 * pct_t8,
         # Using the trapezoidal rule for approximation
         auc_24 = ( (pct_t0 + pct_t8)/2 * 8 + (pct_t8 + pct_t16)/2 * 8 +
                      (pct_t16 + pct_d1)/2 * 8 ),
         second_diff_t0_t16 = (pct_t16 - 2 * pct_t8 + pct_t0) / (8^2),
         slope_diff = (pct_t8 - pct_t0)/8 - (pct_t16 - pct_t8)/8,
         range_0_16 = max(pct_t0, pct_t8, pct_t16) - min(pct_t0, pct_t8, pct_t16),
         pct_avg_16 = (pct_t0 + pct_t8 + pct_t16) / 3,
         auc_rat_16_24_0_24 = ((pct_t16 + pct_d1)/2 * 8) / auc_24,
         val = rel_diff
         )

# ROC anal. wth the 'val' (easily changable) predictor
roc_curve <- roc(inf ~ val, data = dat_full, direction = ">", levels = c(1, 0))

# ROC görbe rajzolása
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
# Add confidence intervals as a shaded area
ci <- ci.se(roc_curve, specificities = seq(0, 1, by = 0.01))  # Specificity range
plot(ci, type = "shape", col = rgb(0.2, 0.5, 0.8, 0.3))  # Add shaded confidence interval

# Define a cutoff percentile for the simple ROC analysis (reproducing orig. findings)
CUTOFF_PERC <- 0.2
cutoff <- quantile(dat_full$val, CUTOFF_PERC, na.rm = TRUE)

dat_full <- dat_full %>%
  mutate(pred = ifelse(val > cutoff,1,0))


# Print AUC
print(auc(roc_curve))
(cutoff)

# Print confusion table (NPV=0.38)
confusionMatrix(
  factor(dat_full$pred),
  factor(dat_full$inf),
  positive = "1")



# Reproduce the "ABSOLUTE" plans
dat_full$pr_simp <- ifelse(dat_full$pct_t0 >0.5,1,0) %>% factor
# Print confusion table (NPV=0.20)
confusionMatrix(dat_full$pr_simp,
                factor(dat_full$inf),
                positive = "0")


# Check for disjointedness in the db
# This is == 0, we are good, each sub is associated with only one row
is_distjunct <-
  dat_full %>%
  filter(is.na(inf) == FALSE) %>%
  select(inf,id) %>%
  # check whether an ID is associated with only one row
  count(id) %>%
  filter(n > 1) %>%
  nrow()


## Fit a decision tree model without any major optimizations

library(rpart)
library(rpart.plot)

colnames(dat_full)

# Define the predictors to use
predictors <- c(
  "age","sex","icu_days", "bdt_t0",
  "baseline_pct",
  "pct_t0",
                # "pct_delta_0_24",
                # "pct_delta_0_24_pct",
                #
                # "pct_delta_16_24","pct_delta_16_24_pct",
                # "pct_delta_8_24_pct",
                # "pct_t8",
                # "pct_t0_log", "pct_t8_log",
                # "pct_t16", "pct_d1",
                # "pct_delta_0_8",
                # "pct_delta_0_16",
                # "pct_delta_8_16",
                # "pct_delta_0_8_pct", "pct_delta_8_16_pct",
                # "pct_delta_0_16_pct",



                "unk_t0"
                # "bdt_t8",
                # "pct_delta_0_8_log",
                # "pct_delta_0_8_perc",
                # "pct_delta_0_8_hand",
                # "cumsum_16",
                # "cumprod_16",
                # "cumsum_8",
                # "cumprod_8",
                # "auc_24",
                # "second_diff_t0_t16",
                # "slope_diff",
                # "range_0_16",
                # "pct_avg_16",
                # "auc_rat_16_24_0_24"
                )

# Replace any NA values for simplicity
dat_clean <- dat_full %>%
  select(inf, all_of(predictors)) %>%
  # for all cols other than inf, impute missings by mean of column
  mutate( across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  filter(inf == round(inf)) %>%
  na.omit() %>%
  mutate(inf = as.factor(inf) %>% forcats::fct_recode("No_infection" = "0",
                                                      "Infection" = "1"))


# Set control parameters for rpart
control_params <- rpart.control(maxdepth = 3, cp = 0, minsplit = 10)

# Build the decision tree model
tree_model <- rpart(
  formula = inf ~ .,
  data = dat_clean,
  method = "class",
  control = control_params,
  parms = list(split = "information")  # Use information gain
)

# Plot the tree (not a ggplot)
rpart.plot(tree_model, type = 2, extra = 101, fallen.leaves = TRUE)

# Make predictions on the training data
predictions <- predict(tree_model, dat_clean) %>%
  {ifelse(.[,2] > 0.5, "Infection", "No_infection")} %>%
  factor()



# Generate a confusion matrix
confusionMatrix(predictions, dat_clean$inf, positive = "Infection")

try({
  fig_5 <-
    dat_clean %>%
      ggplot(.,aes(x = pct_delta_8_24_pct,
                   y = pct_delta_8_16_pct,
                   color = inf, group = inf)) +
      # geom_hist with dodged position
      geom_point(size = 2) +
      geom_vline(xintercept = 90, color = "salmon4") +
      geom_segment(aes(x = 90,  xend = 700,
                       y = 210, yend = 210),
                   linetype = "dashed", color = "salmon4") +
      theme_minimal()
})

# Alternative dec. tree Introduce a heavier penalty for FP

# Define a loss matrix to penalize false positives more
# Has impact
loss_matrix <- matrix(c(0, 1, 2, 0), nrow = 2, byrow = TRUE)
rownames(loss_matrix) <- colnames(loss_matrix) <- c("0", "1")

# Rebuild the model with the loss matrix
tree_model_penal <- rpart(
  formula = inf ~ .,
  data = dat_clean,
  method = "class",
  control = control_params,
  parms = list(loss = loss_matrix, split = "information")
)


# Plot the tree
rpart.plot(tree_model_penal, type = 1, extra = 101, fallen.leaves = TRUE)

# Make predictions on the training data
predictions <- predict(tree_model_penal, dat_clean) %>%
  {ifelse(.[,2] > 0.4, "Infection", "No_infection")} %>%
  factor()

# Generate a confusion matrix
confusionMatrix(predictions, dat_clean$inf, positive = "Infection")

try({
  fig_6 <-
    dat_clean %>%
    ggplot(.,aes(x = pct_delta_16_24_pct,
                 y = pct_delta_8_16_pct,
                 color = inf, group = inf)) +
    # geom_hist with dodged position
    geom_point(size = 2) +
    geom_vline(xintercept = 39, color = "salmon4") +
    theme_minimal()
})


# ###################
# # Optimized decision tree fit (with CrossValid) - This part is commented out since data is unacceptable
#
# library(caret)
# library(DMwR2)
#
# # Define the loss matrix
# loss_matrix <- matrix(
#   c(0, 5,   # Cost of predicting 'Appropriate' when true class is 'Appropriate': 0
#     # Cost of predicting 'Not_appropriate' when true class is 'Appropriate': 5
#     1, 0    # Cost of predicting 'Appropriate' when true class is 'Not_appropriate': 0
#     # Cost of predicting 'Not_appropriate' when true class is 'Not_appropriate': 0
#   ),
#   nrow = 2,
#   byrow = TRUE
# )
#
# # Set row and column names to match the factor levels of 'inf'
# rownames(loss_matrix) <- colnames(loss_matrix) <- c("Appropriate", "Not_appropriate")
#
#
# # Define a custom model to include 'cp', 'maxdepth', and 'minsplit' in tuning
# custom_rpart <- list(
#   type = "Classification",
#   library = "rpart",
#   loop = NULL,
#   parameters = data.frame(
#     parameter = c("cp", "maxdepth", "minsplit"),
#     class = rep("numeric", 3),
#     label = c("Complexity Parameter", "Maximum Depth", "Minimum Split")
#   ),
#   grid = function(x, y, len = NULL, search = "grid") {
#     expand.grid(
#       cp = seq(0.0001, 0.1, length.out = 3),
#       maxdepth = 2:5,
#       minsplit = seq(5, 15, by = 5)
#     )
#   },
#   fit = function(x, y, wts, param, lev, last, classProbs, ...) {
#     rpart::rpart(
#       formula = y ~ .,
#       data = data.frame(x, y),
#       method = "class",
#       control = rpart.control(
#         cp = param$cp,
#         maxdepth = param$maxdepth,
#         minsplit = param$minsplit
#       ),
#       ...
#     )
#   },
#   predict = function(modelFit, newdata, submodels = NULL) {
#     newdata <- as.data.frame(newdata)
#     predict(modelFit, newdata, type = "class")
#   },
#   prob = function(modelFit, newdata, submodels = NULL) {
#     newdata <- as.data.frame(newdata)
#     predict(modelFit, newdata, type = "prob")
#   },
#   levels = function(x) x$ylevels,
#   tags = c("Decision Tree", "Classification"),
#   sort = function(x) x[order(x$cp),]
# )
#
# # Custom summary function to calculate PPV
# ppvSummary <- function(data, lev = NULL, model = NULL) {
#
#   THRESHOLD <- 0.8
#   data <- data %>%
#     mutate(man_pred = ifelse(Appropriate >= THRESHOLD,1,0))
#
#   if (sum(data$man_pred) == 0) {
#     return(c(PPV = 0))
#   }
#
#   res <- 1
#
#   try(silent=TRUE, {
#     tab <- data %>%
#       select(obs, man_pred) %>% table()
#
#     res <- tab %>% {.[1,2]/sum(.[,2])}
#   })
#
#   c(PPV = res)
# }
#
# # Set up cross-validation control
# train_control <- trainControl(
#   method = "cv",
#   number = 10,
#   summaryFunction = ppvSummary,
#   classProbs = TRUE,
#   savePredictions = TRUE,
#   verboseIter = TRUE,
#   ## 'mote' sampling biases the model towards the minority class (lowers NPV!!!)
#   #sampling = "smote" # this is where the sampling DMwR goes
# )
#
# # Define the grid of hyperparameters
# tune_grid <- expand.grid(
#   cp = seq(0.0, 0.05, length.out = 11),
#   maxdepth = 1:4,
#   minsplit = seq(20, 40, by = 5)
#   )
#
# set.seed(123)  # For reproducibility
#
# res_load <- try({
#   load(here::here("data","tree_model_cv.rds"))
# })
#
#
# if ( inherits(res_load, "try-error") ) {
#   tree_model_cv <- train(
#     form = inf ~ .,
#     data = dat_clean,
#     method = custom_rpart,
#     metric = "PPV",
#     trControl = train_control,
#     tuneGrid = tune_grid,
#     parms = list(
#       split = "information",
#       loss = loss_matrix
#       )
#   )
#   save(tree_model_cv, file = here::here("data","tree_model_cv.rds"))
# }
#
#
# # Print the model details
# print(tree_model_cv)
#
#
# # Arm A (KINETICS) NPV: 88.27% (crossvalidated)
# # Arm B (ABSOLUTE) NPV: 30.23%
# # Proportion of negative tests on arm A (KINETICS): 16/141 (11.35%)
# # Proportion of negative tests on arm B (ABSOLUTE): 43/141 (30.5%)
# # True proportion of negatives in the population:   33/141 (23.4%)
#
# # Plot the performance across different 'cp' values
# fig_5 <- plot(tree_model_cv)
#
# # Extract the final model
# final_tree <- tree_model_cv$finalModel
#
# # Plot the final decision tree (NOTE: resampled!)
# rpart.plot(final_tree, type = 2, extra = 101, fallen.leaves = TRUE)
#
#
# # Make predictions on the training data
# predictions <- predict(tree_model_cv, dat_clean)
#
# # Generate a confusion matrix
# confusionMatrix(predictions, dat_clean$inf, positive = "Appropriate")
#
# ######
# # RANDOM FOREST
#
# # Define the grid of 'mtry' values to try
# tune_grid <- expand.grid(
#   mtry = seq(2, (ncol(dat_clean) - 1), by = 1)  # Adjust based on your data
# )
#
#
# set.seed(123)  # For reproducibility
#
# # Fitting a "big forest" for varimp
# rf_model <- train(
#   inf ~ .,
#   data = dat_clean,
#   method = "rf",
#   metric = "PPV",                # Optimize for PPV
#   trControl = train_control,
#   tuneGrid = tune_grid,
#   ntree = 500                    # Number of trees in the forest
# )
#
# print(rf_model)
# plot(rf_model)
#
#
# varImp_rf <- varImp(rf_model, scale = FALSE)
# print(varImp_rf)
# plot(varImp_rf)
#
# # Predictions on training data
# train_preds <- predict(rf_model, dat_clean)
#
# # Confusion matrix
# conf_matrix <- confusionMatrix(train_preds, dat_clean$inf, positive = "Not_appropriate")
# print(conf_matrix)
#
#
# # Reduced RF, with top predictors only; performance still excellent
#
# set.seed(123)  # For reproducibility
#
# # Set up cross-validation control
# train_control <- trainControl(
#   method = "cv",
#   number = 10,
#   summaryFunction = ppvSummary,
#   classProbs = TRUE,
#   savePredictions = TRUE,
#   verboseIter = TRUE,
#   ## 'mote' sampling biases the model towards the minority class (lowers NPV!!!)
#   #sampling = "smote" # this is where the sampling DMwR goes
# )
#
# rf_model_red <- train(
#   inf ~
#     # pct_delta_16_24_pct +
#     # pct_delta_8_24_pct +
#     # auc_rat_16_24_0_24 +
#     # pct_delta_16_24 +
#     pct_delta_0_24_pct +
#     pct_delta_0_24 +
#     #pct_delta_8_16
#     # pct_delta_0_16 +
#     # pct_delta_0_16_pct +
#     # pct_delta_8_16_pct +
#     pct_t0 +
#     # pct_t8 +
#     # pct_delta_0_8 +
#     # pct_delta_0_8_pct +
#      age +
#     # unk_t0 +
#     # bdt_t8 +
#      icu_days
#     # cumsum_8
#   ,
#   data = dat_clean,
#   method = "rf",
#   metric = "PPV",                # Optimize for PPV
#   trControl = train_control,
#   tuneGrid = tune_grid,
#   ntree = 100                    # Number of trees in the forest
# )
#
# print(rf_model_red)
# plot(rf_model_red)
#
# # Predictions on training data
# train_preds <- predict(rf_model_red, dat_clean)
#
# # Confusion matrix
# conf_matrix <- confusionMatrix(train_preds, dat_clean$inf, positive = "Not_appropriate")
# print(conf_matrix)
#
# # AUC analysis
# predictions_prob <- predict(rf_model_red, dat_clean,type="prob")
#
# roc_curve_rf <- roc(dat_clean$inf, predictions_prob$Not_appropriate)
#
# plot(roc_curve_rf)
#
