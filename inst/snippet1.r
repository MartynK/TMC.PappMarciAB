# Set seed for reproducibility
set.seed(123)

# Number of patients for each test
n_test1 <- 200
n_test2 <- 200
prev <- 0.75

# Simulate data for Test 1
# Assume sensitivity = 0.6, specificity = 0.75
# Prevalence of needing antibiotics (disease prevalence)


# Generate true disease status for Test 1
disease_status1 <- rbinom(n_test1, 1, prev)

# Generate test results for Test 1 based on sensitivity and specificity
test1_result <- ifelse(disease_status1 == 1,
                       rbinom(n_test1, 1, 0.6),  # Sensitivity
                       rbinom(n_test1, 1, 1 - 0.75))  # 1 - Specificity

# Simulate data for Test 2
# Assume sensitivity = 0.95, specificity = 0.70

# Generate true disease status for Test 2
disease_status2 <- rbinom(n_test2, 1, prev)

# Generate test results for Test 2 based on sensitivity and specificity
test2_result <- ifelse(disease_status2 == 1,
                       rbinom(n_test2, 1, 0.95),  # Sensitivity
                       rbinom(n_test2, 1, 1 - 0.70))  # 1 - Specificity

# Create data frames
data_test1 <- data.frame(Test = "Test1",
                         Disease = disease_status1,
                         TestResult = test1_result)

data_test2 <- data.frame(Test = "Test2",
                         Disease = disease_status2,
                         TestResult = test2_result)

# Combine datasets
data <- rbind(data_test1, data_test2)

# View the first few rows
head(data)

# Convert variables to factors
data$Test <- factor(data$Test)
data$Disease <- factor(data$Disease, levels = c(0, 1), labels = c("No", "Yes"))
data$TestResult <- factor(data$TestResult, levels = c(0, 1), labels = c("Negative", "Positive"))

# Ensure 'No' (healthy) is the reference level
data$Disease <- relevel(data$Disease, ref = "No")

# Create an interaction term between Test and TestResult
data$Interaction <- interaction(data$Test, data$TestResult)

# Fit logistic regression model
model <- glm(Disease ~ Test * TestResult, data = data, family = binomial)

# Summarize the model
summary(model)

# Install and load emmeans if not already installed
if (!require("emmeans")) {
  install.packages("emmeans")
}
library(emmeans)

# Obtain estimated marginal means
emm <- emmeans(model, ~ Test * TestResult, type = "response")

# View the EMMs
summary(emm)

#### Stupid
# # Perform pairwise comparisons between groups
# pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "bonferroni")
#
# # View the pairwise comparisons
# summary(pairwise_comparisons, infer = TRUE)



# Load necessary package
if (!require("broom")) {
  install.packages("broom")
  library(broom)
}

# Get the exponentiated coefficients (odds ratios)
exp_coefs <- exp(coef(model))

# Calculate confidence intervals
confint_model <- confint(model)

# Exponentiate confidence intervals
exp_confint <- exp(confint_model)

# Create a summary table
summary_table <- data.frame(
  Term = names(coef(model)),
  OR = exp_coefs,
  CI_Lower = exp_confint[, 1],
  CI_Upper = exp_confint[, 2],
  p_value = summary(model)$coefficients[, 4]
)

# View the summary table
print(summary_table)

# Load ggplot2 for plotting
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Create a new data frame for predictions
new_data <- expand.grid(
  Test = factor(c("Test1", "Test2")),
  TestResult = factor(c("Negative", "Positive"), levels = c("Negative", "Positive"))
)

# Predict probabilities
new_data$PredictedProb <- predict(model, newdata = new_data, type = "response")

# Plot the predicted probabilities
ggplot(new_data, aes(x = TestResult, y = PredictedProb, fill = Test)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Predicted Probability of Needing Antibiotics",
       x = "Test Result",
       y = "Predicted Probability") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

# Load pROC package for ROC analysis
if (!require("pROC")) {
  install.packages("pROC")
  library(pROC)
}

# Predict probabilities for all observations
data$PredictedProb <- predict(model, type = "response")

# Compute ROC curve
roc_curve <- roc(data$Disease, data$PredictedProb)

# Plot ROC curve
plot(roc_curve, col = "blue", main = "ROC Curve for Logistic Regression Model")
abline(a = 0, b = 1, lty = 2, col = "gray")

# Calculate Area Under the Curve (AUC)
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))
