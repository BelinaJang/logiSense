library(here)

test_data <- read.csv(here("data", "healthcare-dataset-stroke-data.csv"))

loginterpret <- function(outcome, var_interest, var_type, covariates = c(), data){
  # fit model
  predictors <- append(covariates, var_interest)
  model <- as.formula(paste(outcome,"~",paste(predictors,collapse="+")))
  lrfit <- glm(model, data=data, family = "binomial")

  # extract statistics
  beta <- coef(summary(model))[variable_interest, 'Estimate']
  # calculate OR and 95% CI

  # generate interpretations based on variable type

  # return results
}
