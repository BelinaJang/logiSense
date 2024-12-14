#' @title Logistic Regression Interpretation for One Two-Way Interaction
#' @description a function that returns the interpretation for a logistic regression model with one two-way interaction.
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param variable1 name of the first variable in the interaction of interest
#' @param variable2 name of the second variable in the interaction of interest
#' @param variable1_type type of the first variable in the interaction of interest (either "continuous" or "categorical")
#' @param variable2_type type of the second variable in the interaction of interest (either "continuous" or "categorical")
#' @param data The name of the dataset
#' @return string object delineating interpretation of interaction effects
#' @importFrom broom tidy
#' @examples
#' logint(stroke ~ work_type*age, variable1 = "age", variable2 = "work_type", variable1_type = "continuous", variable2_type = "categorical",data=test_data)
#' logint(formula=stroke ~ age*avg_glucose_level, variable1 = "age", variable2 = "avg_glucose_level", variable1_type = "continuous", variable2_type = "continuous", data = test_data)
#' @export

logint <- function(formula, variable1, variable2, variable1_type, variable2_type, data) {

  # assigning variable1 to appropriate type variable
  if (variable1_type=="continuous"){
    continuous_var <- variable1
  } else if (variable1_type=="categorical"){
    categorical_var <- variable1
  }

  # assigning variable2 to appropriate type variable
  if (variable2_type=="continuous"){
    continuous_var <- variable2
  } else if (variable2_type=="categorical"){
    categorical_var <- variable2
  }

  # run below if one of the types are continuous and the other one is categorical
  if ( (variable1_type=="continuous" & variable2_type=="categorical") | (variable1_type=="categorical" & variable2_type=="continuous")){
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
          "The odds ratio of '", outcome, "' for increasing ", continuous_var, " by one unit in ", categorical_var, " group ",categorical_levels[i]," = ", signif(odds_ratios[i], 4),
          "."
        ), paste0(
          "The 95% CI: e^(", signif(base_effect,4), " ± (1.96)*(",
          signif(vcov_m[odds_ratios_names[1], odds_ratios_names[1]],4), " + ",
          signif(vcov_m[odds_ratios_names[i], odds_ratios_names[i]],4), " + ",
          signif(vcov_m[odds_ratios_names[1], odds_ratios_names[i]],4), ")) = (", signif(CI[1],4), ", ", signif(CI[2],4), ").\n"
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
          categorical_var, " group ", categorical_levels[1]," (reference level) is e^(", signif(categorical_effects[i], 4),
          " + (", signif(interaction_terms[i], 4), ")*(value)", ")."
        ),
        paste0(
          "The 95% CI: e^((", signif(categorical_effects[i], 4),
          " + (", signif(interaction_terms[i], 4), ")*(value)) ± (1.96)*sqrt(",
          signif(vcov_m[categorical_effects_names[i], categorical_effects_names[i]],4), " + ",
          signif(vcov_m[interaction_terms_names[i], interaction_terms_names[i]],4), " + 2*(value)*(",
          signif(vcov_m[categorical_effects_names[i], interaction_terms_names[i]],4), "))). \n"
        )
      )
    }

    # Combine outputs
    full_output <- c(odds_ratio_sentences, "\n", relative_odds_sentences)

    return(cat(full_output, sep = "\n"))
  } else if (variable1_type=="continuous" & variable2_type=="continuous"){
    # run below if both variables are categorical

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
        "For an observation with ", continuous_var2, "=value, the odds ratio of '", outcome, "' for increasing ", continuous_var1, " by one unit is e^(", signif(var1_effect, 4), " + (", signif(interaction_effect, 4), ")*(value)", ")."
      ),
      paste0("The 95% CI: e^((", signif(var1_effect, 4), " + (", signif(interaction_effect, 4), ")*(value)) ± (1.96)*sqrt(", signif(vcov_m[continuous_var1, continuous_var1],4), " + ", signif(vcov_m[interaction_effect_name, interaction_effect_name],4), " + 2*(value)*(", signif(vcov_m[continuous_var1, interaction_effect_name],4), "))). \n"
      ),
      paste0("For an observation with ", continuous_var1, "=value, the odds ratio of '", outcome, "' for increasing ", continuous_var2, " by one unit is e^(", signif(var2_effect, 4), " + (", signif(interaction_effect, 4), ")*(value)", ")."
      ),
      paste0("The 95% CI: e^((", signif(var2_effect, 4), " + (", signif(interaction_effect, 4), ")*(value)) ± (1.96)*sqrt(", signif(vcov_m[continuous_var2, continuous_var2],4), " + ", signif(vcov_m[interaction_effect_name, interaction_effect_name],4), " + 2*(value)*(", signif(vcov_m[continuous_var2, interaction_effect_name],4), "))). \n"
      )
    )
    return(cat(sentences, sep = "\n"))
  }
}
