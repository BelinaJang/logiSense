#' @title Logistic Regression Interpretation for One Two-Way Interaction
#' @description a function that returns the interpretation for a logistic regression model with one two-way interaction of a continuous and a categorical variable.
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param continuous_var The name of the continuous variable
#' @param categorical_var The name of the categorical variable
#' @param data The name of the dataset
#' @param sigfig number of significant figures to round to
#' @return string object delineating interpretation of interaction effects
#' @importFrom broom tidy
#' @examples
#' formula <- stroke ~ work_type*age
#' continuous_var <- "age"
#' categorical_var <- "work_type"
#' data <- test_data
#' sigfig <- 4
#' logint_contbycat(formula, continuous_var, categorical_var, data, sigfig)
#' @export

logint_contbycat <- function(formula, continuous_var, categorical_var, data, sigfig) {

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
  odds_ratio_sentences <- c()

  #using vcov add CI
  vcov_m <- vcov(model)
  odds_ratios_names <- names(odds_ratios)
  for (i in 1:(length(interaction_terms)+1)) {
    CI <- exp(base_effect +c(-1,1)* 1.96 * sqrt(vcov_m[odds_ratios_names[1], odds_ratios_names[1]] + vcov_m[odds_ratios_names[i], odds_ratios_names[i]] + 2*vcov_m[odds_ratios_names[1], odds_ratios_names[i]]))
    odds_ratio_sentences <- c(
      odds_ratio_sentences,
      paste0(
        "The odds ratio of '", outcome, "' for increasing ", continuous_var, " by one unit in ", categorical_var, " group ",categorical_levels[i]," = ", signif(odds_ratios[i], sigfig),
        "."
      ), paste0(
        "The 95% CI: e^(", signif(base_effect,sigfig), " \u00b1 (1.96)*(",
        signif(vcov_m[odds_ratios_names[1], odds_ratios_names[1]],sigfig), " + ",
        signif(vcov_m[odds_ratios_names[i], odds_ratios_names[i]],sigfig), " + ",
        signif(vcov_m[odds_ratios_names[1], odds_ratios_names[i]],sigfig), ")) = (", signif(CI[1],sigfig), ", ", signif(CI[2],sigfig), ").\n"
      )
    )
  }

  categorical_effects_names <- names(categorical_effects)
  interaction_terms_names <- names(interaction_terms)
  # OR for categorical groups relative to baseline, for continuous var
  relative_odds_sentences <- c()
  for (i in seq_along(interaction_terms)) {
    relative_odds_sentences <- c(
      relative_odds_sentences,
      paste0(
        "For an observation with ", continuous_var, "=value, the odds ratio of '", outcome, "' for ",
        categorical_var, " group ", categorical_levels[i + 1], " vs ",
        categorical_var, " group ", categorical_levels[1]," (reference level) is e^(", signif(categorical_effects[i], sigfig),
        " + (", signif(interaction_terms[i], sigfig), ")*(value)", ")."
      ),
      paste0(
        "The 95% CI: e^((", signif(categorical_effects[i], sigfig),
        " + (", signif(interaction_terms[i], sigfig), ")*(value)) \u00b1 (1.96)*sqrt(",
        signif(vcov_m[categorical_effects_names[i], categorical_effects_names[i]],sigfig), " + ",
        signif(vcov_m[interaction_terms_names[i], interaction_terms_names[i]],sigfig), " + 2*(value)*(",
        signif(vcov_m[categorical_effects_names[i], interaction_terms_names[i]],sigfig), "))). \n"
      )
    )
  }

  # Combine outputs
  full_output <- c(odds_ratio_sentences, "\n", relative_odds_sentences)

  return(cat(full_output, sep = "\n"))
}
