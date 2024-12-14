library(tidyverse)
test_data <- read.csv(here("data", "test_data.csv"))

logint_catbycat <- function(formula, var1, var2) {

  model <- glm(formula, data = data, family = binomial)

  variables_list <- as.list(attr(model$terms, "variables"))[-c(1)]
  outcome <- as.character(variables_list[[1]])

  coefficients <- coef(model)

  # warning msg
  if (any(is.na(coefficients))) {
    na_variables <- names(coefficients)[is.na(coefficients)]
    warning(paste0(na_variables," has(have) NA estimates. \n Action Required: Consider re-specifying the model or re-examining interaction terms for meaningfulness. \n"))
  }


}
