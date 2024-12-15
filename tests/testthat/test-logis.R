test_that("logis function handles continuous variable correctly", {
  formula <- stroke ~ gender + age + hypertension + heart_disease + avg_glucose_level + smoking_status
  variable_interest <- "age"
  variable_type <- "continuous"
  data <- test_data
  sigfig <- 4

  expect_output(
    logis(formula, variable_interest, variable_type, data, sigfig),
    "For each one-unit increase in 'age,' the odds of 'stroke' are multiplied by 1\\.072 \\(95% CI: 1\\.061 - 1\\.083\\)\\. This result is statistically significant at 5% significance level \\(p-value: 2\\.346e-40\\)\\."
  )
})

test_that("logis function handles categorical variable correctly", {
  formula <- stroke ~ gender + age + hypertension + heart_disease + avg_glucose_level + smoking_status
  variable_interest <- "smoking_status"
  variable_type <- "categorical"
  data <- test_data
  sigfig <- 4

  expect_output(
    logis(formula, variable_interest, variable_type, data, sigfig),
    "Compared to the reference level 'Unknown' of 'smoking_status,' the odds of 'stroke' for the level 'formerly smoked' are multiplied by 1\\.043 \\(95% CI: 0\\.6979 - 1\\.569\\)\\. This result is not statistically significant at 5% significance level \\(p-value: 0\\.8388\\)\\. \\nCompared to the reference level 'Unknown' of 'smoking_status,' the odds of 'stroke' for the level 'never smoked' are multiplied by 0\\.8514 \\(95% CI: 0\\.5824 - 1\\.257\\)\\. This result is not statistically significant at 5% significance level \\(p-value: 0\\.4115\\)\\. \\nCompared to the reference level 'Unknown' of 'smoking_status,' the odds of 'stroke' for the level 'smokes' are multiplied by 1\\.167 \\(95% CI: 0\\.7415 - 1\\.833\\)\\. This result is not statistically significant at 5% significance level \\(p-value: 0\\.5015\\)\\."
  )
})

test_that("logis function throws error for missing variable of interest", {
  formula <- stroke ~ gender + age + hypertension
  variable_interest <- "unknown_variable"
  variable_type <- "continuous"
  data <- test_data
  sigfig <- 4

  expect_error(
    logis(formula, variable_interest, variable_type, data, sigfig),
    "Variable of interest not found in the model."
  )
})

test_that("logis function throws error for invalid variable type", {
  formula <- stroke ~ gender + age + hypertension
  variable_interest <- "age"
  variable_type <- "invalid_type"
  data <- test_data
  sigfig <- 4

  expect_error(
    logis(formula, variable_interest, variable_type, data, sigfig),
    "Variable type must be either 'continuous' or 'categorical.'"
  )
})
