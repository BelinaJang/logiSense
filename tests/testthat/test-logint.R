test_that("logint handles interaction between a continuous and a categorical variable correctly", {
  formula <- stroke ~ age * work_type
  variable1 <- "age"
  variable2 <- "work_type"
  variable1_type <- "continuous"
  variable2_type <- "categorical"
  data <- test_data
  sigfig <- 4

  expect_output(
    logint(formula, variable1, variable2, variable1_type, variable2_type, data, sigfig),
    "The odds ratio of '",
    fixed = TRUE
  )
})

test_that("logint handles interaction between two continuous variables correctly", {
  formula <- stroke ~ age * avg_glucose_level
  variable1 <- "age"
  variable2 <- "avg_glucose_level"
  variable1_type <- "continuous"
  variable2_type <- "continuous"
  data <- test_data
  sigfig <- 4

  expect_output(
    logint(formula, variable1, variable2, variable1_type, variable2_type, data, sigfig),
    "For an observation with",
    fixed = TRUE
  )
})

test_that("logint handles invalid variable types", {
  formula <- stroke ~ age * work_type
  variable1 <- "age"
  variable2 <- "work_type"
  variable1_type <- "invalid_type"
  variable2_type <- "categorical"
  data <- test_data
  sigfig <- 4

  expect_error(
    logint(formula, variable1, variable2, variable1_type, variable2_type, data, sigfig),
    "variable1_type should be either 'continuous' or 'categorical'."
  )
})

test_that("logint throws an error for missing interaction term", {
  formula <- stroke ~ age*work_type + gender
  variable1 <- "age"
  variable2 <- "gender"
  variable1_type <- "continuous"
  variable2_type <- "categorical"
  data <- test_data
  sigfig <- 4

  expect_error(
    logint(formula, variable1, variable2, variable1_type, variable2_type, data, sigfig),
    "No interaction terms between age and gender found in the model."
  )
})

test_that("logint throws an error for missing interaction term", {
  formula <- stroke ~ age*gender + avg_glucose_level
  variable1 <- "age"
  variable2 <- "avg_glucose_level"
  variable1_type <- "continuous"
  variable2_type <- "continuous"
  data <- test_data
  sigfig <- 4

  expect_error(
    logint(formula, variable1, variable2, variable1_type, variable2_type, data, sigfig),
    "No interaction terms between age and avg_glucose_level found in the model."
  )
})

