#' @title Logistic Regression Interpretation for One Two-Way Interaction
#' @description a function that returns the interpretation for a logistic regression model with one two-way interaction of two continuous variables.
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param variable1 The name of first continuous variable
#' @param variable2 The name of second continuous variable
#' @param data The name of the dataset
#' @param sigfig number of significant figures to round to
#' @return string object delineating interpretation of interaction effects
#' @importFrom broom tidy
#' @examples
#' logint_contbycont(formula=stroke ~ age*avg_glucose_level, variable1 = "age", variable2 = "avg_glucose_level", data = test_data, sigfig=4)
#' @export

logint_contbycont <- function(formula, variable1, variable2, data, sigfig=4) {
  continuous_var1 <- variable1
  continuous_var2 <- variable2

  cat("continuous_var1:", continuous_var1, "continuous_var2:", continuous_var2, "\n")
  model <- glm(formula, data = data, family = binomial)

  variables_list <- as.list(attr(model$terms, "variables"))[-c(1)]
  outcome <- as.character(variables_list[[1]])
  coefficients <- coef(model)

  var1_effect <- coefficients[continuous_var1]
  var2_effect <- coefficients[continuous_var2]
  interaction_effect <- coefficients[grep(":", names(coefficients))]


  # if any coefficients is NA, return warning message
  if (any(is.na(coefficients))) {
    na_variables <- names(coefficients)[is.na(coefficients)]
    warning(paste0(na_variables," has(have) NA estimates. \n Action Required: Consider re-specifying the model or re-examining interaction terms for meaningfulness. \n"))
  }

  interaction_effect_name <- names(interaction_effect)

  vcov_m <- vcov(model)

  sentences <- c(
    paste0(
      "For an observation with ", continuous_var2, "=value, the odds ratio of '", outcome, "' for increasing ", continuous_var1, " by one unit is e^(", signif(var1_effect, sigfig), " + (", signif(interaction_effect, sigfig), ")*(value)", ")."
    ),
    paste0("The 95% CI: e^((", signif(var1_effect, sigfig), " + (", signif(interaction_effect, sigfig), ")*(value)) ± (1.96)*sqrt(", signif(vcov_m[continuous_var1, continuous_var1],sigfig), " + ", signif(vcov_m[interaction_effect_name, interaction_effect_name],sigfig), " + 2*(value)*(", signif(vcov_m[continuous_var1, interaction_effect_name],sigfig), "))). \n"
    ),
    paste0("For an observation with ", continuous_var1, "=value, the odds ratio of '", outcome, "' for increasing ", continuous_var2, " by one unit is e^(", signif(var2_effect, sigfig), " + (", signif(interaction_effect, sigfig), ")*(value)", ")."
    ),
    paste0("The 95% CI: e^((", signif(var2_effect, sigfig), " + (", signif(interaction_effect, sigfig), ")*(value)) ± (1.96)*sqrt(", signif(vcov_m[continuous_var2, continuous_var2],sigfig), " + ", signif(vcov_m[interaction_effect_name, interaction_effect_name],sigfig), " + 2*(value)*(", signif(vcov_m[continuous_var2, interaction_effect_name],sigfig), "))). \n"
    )
  )
  return(cat(sentences, sep = "\n"))
}
