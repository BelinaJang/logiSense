test_that("logint_contbycont handles interaction between two continuous variables correctly", {
  formula <- stroke ~ age*avg_glucose_level
  variable1 <- "age"
  variable2 <- "avg_glucose_level"
  data <- test_data
  sigfig <- 4

  expect_output(
    logint_contbycont(formula, variable1, variable2, data, sigfig),
    "For an observation with",
    fixed = TRUE
  )
})

test_that("logint_contbycont throws an error for missing interaction term", {
  formula <- stroke ~ age*gender + avg_glucose_level
  variable1 <- "age"
  variable2 <- "avg_glucose_level"
  data <- test_data
  sigfig <- 4

  expect_error(
    logint_contbycont(formula, variable1, variable2, data, sigfig),
    "No interaction terms between age and avg_glucose_level found in the model."
  )
})
