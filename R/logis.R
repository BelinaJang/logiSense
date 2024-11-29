#' @title logistic regression interpretation function without interaction
#' @description a function that returns the interpretation of logistic regression results
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.
#' @param variable_interest
#' @param variable_type
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which glm is called.
#' @return
#' @importFrom tidy
#' @examples
#' coef <- c(0.6262892, 0.1045653, -0.7847091)
#' se <- c(-1.253949, -1.245193, 0.423182)
#' siglevel <- 0.05
#' roundto <- 3
#' OR_95CI(coef, se, siglevel, roundto)
#' @export

logis <- function(formula, variable_interest, variable_type, data) {

  model <- glm(formula, data = data, family = binomial)

  variables_list <- as.list(attr(model$terms, "variables"))[-c(1)]
  outcome <- as.character(variables_list[[1]])
  cat("Outcome variable: ", outcome, "\n")

  result <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
  result_terms <- result$term

  interpretation <- ""

  if (variable_type == "continuous") {
    vi_result <- result[result$term == variable_interest, ]
    if (nrow(vi_result) == 0) {
      stop("Variable of interest not found in the model.")
    }

    estimate <- vi_result$estimate
    ci_lower <- vi_result$conf.low
    ci_upper <- vi_result$conf.high
    p_value <- vi_result$p.value

    ci_lower <- ifelse(is.na(ci_lower) | is.infinite(ci_lower), "NA", signif(ci_lower, 4))
    ci_upper <- ifelse(is.na(ci_upper) | is.infinite(ci_upper), "NA", signif(ci_upper, 4))

    p_interpretation <- if (p_value < 0.05) {
      paste0("This result is statistically significant at 5% significance level (p-value: ", signif(p_value, 4), ").")
    } else {
      paste0("This result is not statistically significant at 5% significance level (p-value: ", signif(p_value, 4), ").")
    }

    interpretation <- paste0(
      interpretation,
      "For each one-unit increase in '", variable_interest, ",' the odds of '", outcome,
      "' are multiplied by ", signif(estimate, 4),
      " (95% CI: ", signif(ci_lower, 4), " - ", signif(ci_upper, 4), "). ",
      p_interpretation, "\n"
    )
  } else if (variable_type == "categorical") {
    data[[variable_interest]] <- factor(data[[variable_interest]])
    levels <- levels(data[[variable_interest]])
    reference_level <- levels[1]
    cat("Reference level: ", reference_level, "\n")

    for (comparison_level in levels[-1]) {
      term_name <- paste0(variable_interest, comparison_level)
      vi_result <- result[result$term == term_name, ]
      if (nrow(vi_result) > 0) {
        estimate <- vi_result$estimate
        ci_lower <- vi_result$conf.low
        ci_upper <- vi_result$conf.high
        p_value <- vi_result$p.value

        ci_lower <- ifelse(is.na(ci_lower) | is.infinite(ci_lower), "NA", signif(ci_lower, 4))
        ci_upper <- ifelse(is.na(ci_upper) | is.infinite(ci_upper), "NA", signif(ci_upper, 4))

        p_interpretation <- if (p_value < 0.05) {
          paste0("This result is statistically significant at 5% significance level (p-value: ", signif(p_value, 4), "). ")
        } else {
          paste0("This result is not statistically significant at 5% significance level (p-value: ", signif(p_value, 4), "). ")
        }

        interpretation <- paste0(
          interpretation,
          "Compared to the reference level '", reference_level, "' of '",
          variable_interest,
          ",' the odds of '", outcome, "' for the level '", comparison_level,
          "' are multiplied by ", signif(estimate, 4),
          " (95% CI: ", ci_lower, " - ", ci_upper, "). ",
          p_interpretation, "\n"
        )
      }
    }
  } else {
    stop("Variable type must be either 'continuous' or 'categorical.'")
  }
  return(interpretation)
}
