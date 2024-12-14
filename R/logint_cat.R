library(tidyverse)
test_data <- read.csv(here("data", "test_data.csv"))

logint_catbycat <- function(formula, variable1, variable2, variable1_type, variable2_type, data, sigfig = 4) {
  # init -- go in wrapper
  model <- glm(formula, data = data, family = binomial)

  variables_list <- as.list(attr(model$terms, "variables"))[-c(1)]
  outcome <- as.character(variables_list[[1]])
  coefficients <- coef(model)


  interaction_effect <- coefficients[grep(":", names(coefficients))]

  # assigning variable1 to appropriate type variable
  if (variable1_type=="continuous"){
    continuous_var1 <- variable1
  } else if (variable1_type=="categorical"){
    categorical_var1 <- variable1
  }

  # assigning variable2 to appropriate type variable
  if (variable2_type=="continuous"){
    continuous_var2 <- variable2
  } else if (variable2_type=="categorical"){
    categorical_var2 <- variable2
  }

  var1_effect <- coefficients[categorical_var1]
  var2_effect <- coefficients[categorical_var2]
  interaction_effect <- coefficients[grep(":", names(coefficients))]

  ###### run below #############
  if (variable1_type == 'categorical' & variable2_type == "categorical") {

  }
}
