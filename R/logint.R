logint <- function(formula, continuous_var, categorical_var, data) {

  model <- glm(formula, data = data, family = binomial)

  variables_list <- as.list(attr(model$terms, "variables"))[-c(1)]
  outcome <- as.character(variables_list[[1]])

  coefficients <- coef(model)

  # if any coefficeitns is NA, return warning message
  if (any(is.na(coefficients))) {
    na_variables <- names(coefficients)[is.na(coefficients)]
    warning(paste0(na_variables," has(have) NA estimates. \n Action Required: Consider re-specifying the model or re-examining interaction terms for meaningfulness. \n"))
  }

  # Get the base odds ratio for the continuous variable
  base_effect <- coefficients[continuous_var]

  # Extract interaction terms
  #interaction_terms <- coefficients[grep(paste0(continuous_var, ":"), names(coefficients))]
  result <- tidy(model)
  interaction_terms <- coefficients[result[grep(":", result$term), ]$term]

  # Extract main effects for the categorical variable (excluding baseline)
  categorical_effects <- coefficients[grep(paste0("^", categorical_var), names(coefficients))]

  # Extract levels of the categorical variable
  categorical_levels <- levels(as.factor(model$model[[categorical_var]]))

  # OR for increasing continuous var by 1
  odds_ratios <- c(exp(base_effect), exp(base_effect + interaction_terms))

  # OR for increasing continuous var by 1, in categorical group
  odds_ratio_sentences <- c(
    paste0("The odds ratio of '", outcome,
           "' for increasing ", continuous_var, " by one unit in ", categorical_levels[1], " = ", signif(odds_ratios[1], 4))
  )

  for (i in seq_along(interaction_terms)) {
    odds_ratio_sentences <- c(
      odds_ratio_sentences,
      paste0(
        "The odds ratio of '", outcome,
        "' for increasing ", continuous_var, " by one unit in ",
        categorical_levels[i + 1], " = ", signif(odds_ratios[i + 1], 4)
      )
    )
  }

  # OR for categorical groups relative to baseline, for continuous var
  relative_odds_sentences <- c()
  for (i in seq_along(interaction_terms)) {
    relative_odds_sentences <- c(
      relative_odds_sentences,
      paste0(
        "For an observation with ", continuous_var, "=value, the odds ratio of '", outcome, "' for ",
        categorical_levels[i + 1], " vs Baseline is e^(", signif(categorical_effects[i], 4),
        " + ", signif(interaction_terms[i], 4), "*(value)", ")."
      )
    )
  }

  # Combine outputs
  full_output <- c(odds_ratio_sentences, relative_odds_sentences)

  return(cat(full_output, sep = "\n"))
}
