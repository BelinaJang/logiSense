library(tidyverse)
test_data <- read.csv(here("data", "test_data.csv"))

logint_catbycat <- function(formula, variable1, variable2, variable1_type, variable2_type, data, sigfig = 4) {
  ################# init -- go in wrapper #######################
  model <- glm(formula, data = data, family = binomial)

  variables_list <- as.list(attr(model$terms, "variables"))[-c(1)]
  outcome <- as.character(variables_list[[1]])
  coefficients <- coef(model)

  interaction_effect <- coefficients[grep(":", names(coefficients))]

  # NA msg
  if (any(is.na(coefficients))) {
    na_variables <- names(coefficients)[is.na(coefficients)]
    warning(paste0(na_variables," has(have) NA estimates. \n Action Required: Consider re-specifying the model or re-examining interaction terms for meaningfulness. \n"))
  }


# # assigning variable1 to appropriate type variable


  # assigning variable1 to appropriate type variable
  if (variable1_type=="continuous"){
    continuous_var1 <- variable1
  } else if (variable1_type=="categorical"){
    categorical_var1 <- variable1
  }


  # if (variable1_type=="continuous"){
  #   continuous_var1 <- variable1
  # } else if (variable1_type=="categorical"){
  #   categorical_var1 <- variable1
  # }
  #
  # # assigning variable2 to appropriate type variable
  # if (variable2_type=="continuous"){
  #   continuous_var2 <- variable2
  # } else if (variable2_type=="categorical"){
  #   categorical_var2 <- variable2
  # }


################### fcn below ############# (args: variables list, outcome, coefficients, catvar1, catvar2)

  if (variable1_type == 'categorical' & variable2_type == "categorical") {
    var1_effect <- coefficients[categorical_var1]
    var2_effect <- coefficients[categorical_var2]
    interaction_effect <- coefficients[grep(":", names(coefficients))]

    result <- tidy(model)
    interaction_terms <- coefficients[result[grep(":", result$term), ]$term]

    intercept <- coefficients[1]

    cat_effects1 <- coefficients[grep(paste0("^", categorical_var1), names(coefficients))]
    cat_effects2 <- coefficients[grep(paste0("^", categorical_var2), names(coefficients))]

    cat_levels1 <- levels(as.factor(model$model[[categorical_var1]]))
    cat_levels2 <- levels(as.factor(model$model[[categorical_var2]]))

    # OR(s)
    vcov_m <- vcov(model)
    odds_ratios <- c(exp(intercept), exp(intercept + interaction_terms))
    odds_ratios_names <- names(odds_ratios)
    odds_ratio_sentences <- c()

    for (i in seq_along(interaction_terms)) {
      # 95% confint
      var <- vcov_m[odds_ratios_names[i], odds_ratios_names[i]]
      se <- sqrt(var)
      ci <- exp(base_effect + c(-1,1) * 1.96 * se)

      # sentences
      odds_ratio_sentences <- c(
        odds_ratio_sentences,
        paste0(
          "The odds ratio for the outcome '", outcome, "' when comparing the group ",
          categorical_var1," level '", cat_levels1[i], "' with the reference level, ",
          "in the context of ", categorical_var2, " level '", cat_levels2[i],
          "' is", signif(odds_ratios[i], sigfig), "."
        ),
        paste0(
          "The 95% CI for this odds ratio is: [", signif(CI[1], sigfig),
          ", ", signif(CI[2], sigfig),"].\n"
        )
      )
    }

  }
}



######## tests OR(s)
vcov_m <- vcov(model)
odds_ratios <- c(exp(intercept), exp(intercept + interaction_terms))
odds_ratios_names <- names(odds_ratios)
odds_ratio_sentences <- c()

for (i in seq_along(interaction_terms)) {
  # 95% confint
  var <- vcov_m[odds_ratios_names[i], odds_ratios_names[i]]
  se <- sqrt(var)
  ci <- exp(intercept + c(-1,1) * 1.96 * se)

  # sentences
  odds_ratio_sentences <- c(
    odds_ratio_sentences,
    paste0(
      "The odds ratio for the outcome '", outcome, "' when comparing the group ",
      categorical_var1," level '", cat_levels1[i], "' with the reference level, ",
      "in the context of ", categorical_var2, " level '", cat_levels2[i],
      "' is ", signif(odds_ratios[i], sigfig), "."
    ),
    paste0(
      "The 95% CI for this odds ratio is: [", signif(ci[1], sigfig),
      ", ", signif(ci[2], sigfig),"].\n"
    )
  )
}
