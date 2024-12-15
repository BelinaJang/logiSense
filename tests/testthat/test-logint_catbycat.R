test_that("logint_catbycat handles interaction between two categorical variables correctly", {
  formula <- stroke ~ work_type * Residence_type
  variable1 <- "work_type"
  variable2 <- "Residence_type"
  data <- test_data
  sigfig <- 4

  expect_output(
    logint_catbycat(formula, variable1,variable2, data, sigfig),
    "The odds ratio of ",
    fixed = TRUE
  )
})

test_that("logint_catbycat handles interaction between two categorical variables correctly even when the two variables are not in the right order", {
  formula <- stroke ~ work_type * Residence_type
  variable1 <- "Residence_type"
  variable2 <- "work_type"
  data <- test_data
  sigfig <- 4

  expect_output(
    logint_catbycat(formula, variable1,variable2, data, sigfig),
    "The odds ratio of ",
    fixed = TRUE
  )
})

test_that("logint_catbycat throws an error for missing interaction term", {
  formula <- stroke ~ age*work_type + Residence_type
  variable1 <- "work_type"
  variable2 <- "Residence_type"
  data <- test_data
  sigfig <- 4

  expect_error(
    logint_contbycat(formula, variable1,variable2, data, sigfig),
    "No interaction terms between work_type and Residence_type found in the model."
  )
})
