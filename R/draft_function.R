library(here)

test_data <- read.csv(here("data", "healthcare-dataset-stroke-data.csv"))

glm_model <- glm(stroke ~ gender + age + hypertension + heart_disease +
                   avg_glucose_level + smoking_status + Residence_type, family = binomial, data = test_data)

#return column names
variables_list <- as.list(attr(glm_model$terms, "variables"))[-c(1)]

for (i in length(coef(glm_model))){
  # for variables
  var <- as.character(variables_list[i])
  # extract coefficient of the variable's reference level
  coef <- coef(glm_model)[i]

  # for interaction
}

#return coefficient of age
coef_age <- coef(glm_model)["age"]

# return the reference level of gender

levels(test_data$gender)
