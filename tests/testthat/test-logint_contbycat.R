test_that("logint_contbycat handles interaction between a continuous and a categorical variable correctly", {
  formula <- stroke ~ age * work_type
  continuous_var <- "age"
  categorical_var <- "work_type"
  data <- test_data
  sigfig <- 4

  expect_output(
    logint_contbycat(formula, continuous_var,categorical_var, data, sigfig),
    "The odds ratio of",
    fixed = TRUE
  )
})

test_that("logint_contbycat throws an error for missing interaction term", {
  formula <- stroke ~ age*work_type + gender
  continuous_var <- "age"
  categorical_var <- "gender"
  data <- test_data
  sigfig <- 4

  expect_error(
    logint_contbycat(formula, continuous_var,categorical_var, data, sigfig),
    "No interaction terms between age and gender found in the model."
  )
})
