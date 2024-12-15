#' @title Logistic Regression Interpretation for One Two-Way Interaction
#' @description a function that returns the interpretation for a logistic regression model with one two-way interaction.
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param variable1 name of the first variable in the interaction of interest
#' @param variable2 name of the second variable in the interaction of interest
#' @param variable1_type type of the first variable in the interaction of interest (either "continuous" or "categorical")
#' @param variable2_type type of the second variable in the interaction of interest (either "continuous" or "categorical")
#' @param data The name of the dataset
#' @param sigfig number of significant figures to round to
#' @return `logint3` prints the interpretation of the result directly to the console using "cat()"
#' @importFrom broom tidy
#' @importFrom stats glm binomial coef vcov
#' @examples
#' ## Example 1: Interaction between a continuous and a categorical variable
#' formula <- stroke ~ work_type*age
#' variable1 <- "age"
#' variable2 <- "work_type"
#' variable1_type <- "continuous"
#' variable2_type <- "categorical"
#' data <- test_data
#' sigfig <- 4
#' logint3(formula, variable1, variable2, variable1_type, variable2_type, data, sigfig)
#' ## Example 2: Interaction between two continuous variables
#' formula <- stroke ~ age*avg_glucose_level
#' variable1 <- "age"
#' variable2 <- "avg_glucose_level"
#' variable1_type <- "continuous"
#' variable2_type <- "continuous"
#' data <- test_data
#' sigfig <- 4
#' logint3(formula, variable1, variable2, variable1_type, variable2_type, data, sigfig)
#' @export

logint3 <- function(formula, variable1, variable2, variable1_type, variable2_type, data, sigfig=4) {

  #####################################################
  if (variable1_type == "continuous" & variable2_type == "continuous"){
    #both continuous
    continuous_var1 <- variable1
    continuous_var2 <- variable2
    message("Both variables are continuous. \n")
    message("continuous_var1: ", continuous_var1, ", continuous_var2: ", continuous_var2, "\n")

  } else if (variable1_type == "categorical" & variable2_type == "categorical"){
    #both categorical
    categorical_var1 <- variable1
    categorical_var2 <- variable2
    message("Both variables are categorical. \n")
    message("categorical_var1: ", categorical_var1, ", categorical_var2: ", categorical_var2, "\n")

  } else if (variable1_type != variable2_type){
    # when two variables are different types, assign each variable to appropriate type variable
    message("Both variables are of different types. \n")

    if (variable1_type == "continuous"){
      continuous_var <- variable1
      #message("Variable 1 is continuous. \n")
      message("continuous_var: ", continuous_var, "\n")
    } else if (variable1_type == "categorical"){
      categorical_var <- variable1
      #message("Variable 1 is categorical. \n")
      message("categorical_var: ", categorical_var, "\n")
    } else {
      stop("variable1_type should be either 'continuous' or 'categorical'.")
    }
    if (variable2_type == "continuous"){
      continuous_var <- variable2
      #message("Variable 2 is continuous. \n")
      message("continuous_var: ", continuous_var, "\n")
    } else if (variable2_type == "categorical"){
      categorical_var <- variable2
      #message("Variable 2 is categorical. \n")
      message("categorical_var: ", categorical_var, "\n")
    } else {
      stop("variable2_type should be either 'continuous' or 'categorical'.")
    }
  }
  #####################################################

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
    result <- tidy(model)
    #interaction_terms <- coefficients[result[grep(":", result$term), ]$term] - wrong

    # error handling added
    result_terms <- result$term

    # find terms that include both continuous and categorical variables from result_terms
    interaction_terms <- coefficients[result_terms[sapply(result_terms, function(term) {
      all(sapply(c(continuous_var,categorical_var), function(var) grepl(var, term)))
    })]]

    if (length(interaction_terms) == 0){
      stop("No interaction terms between ", continuous_var, " and ", categorical_var, " found in the model.")
    }

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

  } else if (variable1_type=="continuous" & variable2_type=="continuous"){
    # run below if both variables are continuous

    model <- glm(formula, data = data, family = binomial)

    variables_list <- as.list(attr(model$terms, "variables"))[-c(1)]

    outcome <- as.character(variables_list[[1]])
    coefficients <- coef(model)

    var1_effect <- coefficients[continuous_var1]
    var2_effect <- coefficients[continuous_var2]
    #interaction_effect <- coefficients[grep(":", names(coefficients))] - wrong

    # error handling added
    result <- tidy(model)
    result_terms <- result$term

    # find terms that include both continuous and categorical variables from result_terms
    interaction_effect <- coefficients[result_terms[sapply(result_terms, function(term) {all(sapply(c(continuous_var1,continuous_var2), function(var) grepl(var, term)))})]]

    if (length(interaction_effect) == 0){
      stop("No interaction terms between ", continuous_var1, " and ", continuous_var2, " found in the model.")
    }

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
      paste0("The 95% CI: e^((", signif(var1_effect, sigfig), " + (", signif(interaction_effect, sigfig), ")*(value)) \u00b1 (1.96)*sqrt(", signif(vcov_m[continuous_var1, continuous_var1],sigfig), " + ", signif(vcov_m[interaction_effect_name, interaction_effect_name],sigfig), " + 2*(value)*(", signif(vcov_m[continuous_var1, interaction_effect_name],sigfig), "))). \n"
      ),
      paste0("For an observation with ", continuous_var1, "=value, the odds ratio of '", outcome, "' for increasing ", continuous_var2, " by one unit is e^(", signif(var2_effect, sigfig), " + (", signif(interaction_effect, sigfig), ")*(value)", ")."
      ),
      paste0("The 95% CI: e^((", signif(var2_effect, sigfig), " + (", signif(interaction_effect, sigfig), ")*(value)) \u00b1 (1.96)*sqrt(", signif(vcov_m[continuous_var2, continuous_var2],sigfig), " + ", signif(vcov_m[interaction_effect_name, interaction_effect_name],sigfig), " + 2*(value)*(", signif(vcov_m[continuous_var2, interaction_effect_name],sigfig), "))). \n"
      )
    )
    return(cat(sentences, sep = "\n"))
  } else if (variable1_type=="categorical" & variable2_type=="categorical"){
    logint_catbycat(formula, variable1, variable2, data, sigfig)
  }
}
