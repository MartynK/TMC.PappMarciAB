library(ggplot2)
library(dplyr)
library(pROC)
library(randomForest)
library(caret)

source(here::here("inst","functions","load_stuff.r"))
#load(here::here("data","dat_full.rds"))

dat_full <- readxl::read_xlsx(here::here("inst","extdata",
                                         "2024.12.21","t1t0.xlsx")) %>%
  rename( t_min_1 = `T-1`,
          inf = status) %>%
  mutate( inf = as.factor(inf) %>% forcats::fct_recode( "0" = "no_infection" ,
                                                        "1" = "infection" )
  )

# Calculate a bunch of new variables
dat_full <- dat_full %>%
  mutate(
    pct_t0_log = log(t0),
    val = ratio_over_diff
  )

## Fit a decision tree model without any major optimizations

library(rpart)
library(rpart.plot)


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



###################
# Optimized decision tree fit (with CrossValid)

library(caret)
library(DMwR2)

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


# Define a custom model to include 'cp', 'maxdepth', and 'minsplit' in tuning
custom_rpart <- list(
  type = "Classification",
  library = "rpart",
  loop = NULL,
  parameters = data.frame(
    parameter = c("cp", "maxdepth", "minsplit"),
    class = rep("numeric", 3),
    label = c("Complexity Parameter", "Maximum Depth", "Minimum Split")
  ),
  grid = function(x, y, len = NULL, search = "grid") {
    expand.grid(
      cp = seq(0.0001, 0.1, length.out = 3),
      maxdepth = 2:5,
      minsplit = seq(5, 15, by = 5)
    )
  },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    rpart::rpart(
      formula = y ~ .,
      data = data.frame(x, y),
      method = "class",
      control = rpart.control(
        cp = param$cp,
        maxdepth = param$maxdepth,
        minsplit = param$minsplit
      ),
      ...
    )
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, type = "class")
  },
  prob = function(modelFit, newdata, submodels = NULL) {
    newdata <- as.data.frame(newdata)
    predict(modelFit, newdata, type = "prob")
  },
  levels = function(x) x$ylevels,
  tags = c("Decision Tree", "Classification"),
  sort = function(x) x[order(x$cp),]
)

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

set.seed(123)  # For reproducibility


  tree_model_cv <- train(
    form = inf ~ .,
    data = dat_clean,
    method = custom_rpart,
    metric = "NPV",
    trControl = train_control,
    tuneGrid = tune_grid,
    parms = list(
      split = "information",
      loss = loss_matrix
    )
  )
