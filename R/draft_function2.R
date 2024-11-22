library(here)

test_data <- read.csv(here("data", "healthcare-dataset-stroke-data.csv"))

glm_model <- glm(stroke ~ gender + age + hypertension + heart_disease +
                   avg_glucose_level + smoking_status + Residence_type, family = binomial, data = test_data)

# input
outcome <- "stroke"
var_interest <- "gender + age"
covariates <- c("gender","age")

lrfd <- function(outcome,var,covariates, data){
  formula <- as.formula(paste(outcome, "~", paste(covariates, collapse = " + ")))
  model <- glm(formula, data = data, family = binomial)

  variables_list <- as.list(attr(model$terms, "variables"))[-c(1)]

  for (i in length(coef(glm_model))){
    # for individual variables
    if (i<=length(variables_list)){
      if (i == 1){
        var <- "intercept"
        coef <- coef(glm_model)[i]
      }
      else {
        var <- as.character(variables_list[i])
        coef <- coef(glm_model)[i]
      }
    }
    else if (i>length(variables_list) and i<=length(coef(glm_model))){
      # for interaction
    }
  }

}

result <- lrfd(outcome, var_interest, covariates, test_data)
result
