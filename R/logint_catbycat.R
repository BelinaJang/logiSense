#' @title Logistic Regression Interpretation for Categorical by Categorical Interaction
#' @description a function that returns the interpretation for a logistic regression model with one two-way interaction of a categorical and a categorical variable.
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param variable1 The name of the first categorical variable in the interaction
#' @param variable2 The name of the second categorical variable in the interaction
#' @param data The name of the dataset
#' @param sigfig integer indicating the number of significant digits to be used.
#' @return `logint_catbycat` prints the interpretation of the result directly to the console using "cat()"
#' @importFrom broom tidy
#' @importFrom stats glm binomial coef vcov
#' @examples
#' formula <- stroke ~ work_type*Residence_type
#' variable1 <- "work_type"
#' variable2 <- "Residence_type"
#' data <- test_data
#' sigfig <- 4
#' logint_catbycat(formula, variable1, variable2, data, sigfig)
#' @export
logint_catbycat <- function(formula, variable1, variable2, data, sigfig = 4) {

  model <- glm(formula, data = data, family = binomial)
  outcome <- as.character(attr(model$terms, "variables")[-c(1)][1])
  coefficients <- coef(model)

  #### error handling added ####
  result <- tidy(model)
  result_terms <- result$term

  # find terms that include both categorical and categorical variables from result_terms
  interaction_terms_check <- coefficients[result_terms[sapply(result_terms, function(term) {
    all(sapply(c(variable1,variable2), function(var) grepl(var, term)))
  })]]

  if (length(interaction_terms_check) == 0){
    stop("No interaction terms between ", variable1, " and ", variable2, " found in the model.")
  }

  ######switch variable 1 and variable 2 if variable 2 appears before variable 1######
  term_order <- attr(model$terms, "term.labels")
  interaction_term <- paste0(variable1, ":", variable2)
  reverse_interaction_term <- paste0(variable2, ":", variable1)

  if (reverse_interaction_term %in% term_order && !(interaction_term %in% term_order)) {
    message("Switching variable1 and variable2 for consistent interpretation.")
    temp <- variable1
    variable1 <- variable2
    variable2 <- temp
  }

  #############################

  if (any(is.na(coefficients))) {
    na_variables <- names(coefficients)[is.na(coefficients)]
    warning(paste0(na_variables," has(have) NA estimates. \n Action Required: Consider re-specifying the model or re-examining interaction terms for meaningfulness. \n"))
  }

  levels1 <- levels(as.factor(model$model[[variable1]]))
  levels2 <- levels(as.factor(model$model[[variable2]]))

  ref_level1 <- levels1[1]
  ref_level2 <- levels2[1]

  coef_model <- coef(summary(model))
  beta_intercept <- coefficients[1]

  sentences <- c()

  # Given variable 2
  for (i in levels1[-1]) {
    for (j in levels2) {
      main1 <- paste0(variable1, i)
      main2 <- paste0(variable2, j)
      interaction_term <- paste0(variable1, i, ":", variable2, j)

      message(paste0("interaction: ", interaction_term))

      beta_main1 <- ifelse(main1 %in% rownames(coef_model), coef_model[main1, "Estimate"], 0)
      beta_main2 <- ifelse(main2 %in% rownames(coef_model), coef_model[main2, "Estimate"], 0)
      beta_interaction <- ifelse(interaction_term %in% rownames(coef_model),
                                 coef_model[interaction_term, "Estimate"], 0)

      log_odds <- beta_main1 + beta_interaction
      odds_ratio <- exp(log_odds)

      message(paste0("(i,j): (", i,',', j, "), beta_main1:", beta_main1, ", beta_main2:", beta_main2,
                   ", beta_interaction:", beta_interaction, ", log_odds:", log_odds))

      vcov_m <- vcov(model)

      sentences <- c(
        sentences,
        #paste0(
        #  "The odds ratio for the outcome '", outcome, "' when comparing the group ",
        #  variable1," level '", i, "' with the reference level, (",
        #  ref_level1, ") in the context of ", variable2, " level '",
        #  j,"' is ", signif(odds_ratio, sigfig), "."
        #)
        paste0(
          "The odds ratio of '", outcome, "' for the ",
          variable1," group ", i, " vs ",variable1," group ", ref_level1," (reference level) in the context of ",variable2, " group ", j," is ",signif(odds_ratio, sigfig), "."
        )
      )

      if(j==ref_level2){
        sentences <- c(
          sentences,
          paste0(
            "The 95% CI: (",signif(exp(log_odds + (-1.96) * sqrt(vcov_m[main1, main1])),sigfig),", ",signif(exp(log_odds + 1.96 * sqrt(vcov_m[main1, main1])),sigfig),")"
          )
        )
      } else {
        sentences <- c(
          sentences,
          paste0(
            "The 95% CI: (", signif(exp(log_odds + (-1.96) * sqrt(vcov_m[main1, main1] + vcov_m[interaction_term, interaction_term] + 2*vcov_m[main1,interaction_term])),sigfig),
            ", ", signif(exp(log_odds + 1.96 * sqrt(vcov_m[main1, main1] + vcov_m[interaction_term, interaction_term] + 2*vcov_m[main1,interaction_term])),sigfig),")"
          )
        )
      }
    }
  }

  # Given variable 1
  for (j in levels2[-1]) {
    for (i in levels1) {
      main1 <- paste0(variable1, i)
      main2 <- paste0(variable2, j)
      interaction_term <- paste0(variable1, i, ":", variable2, j)

      message(paste0("main1:", main1, " main2:", main2, " interaction:", interaction_term))


      beta_main1 <- ifelse(main1 %in% rownames(coef_model), coef_model[main1, "Estimate"], 0)
      beta_main2 <- ifelse(main2 %in% rownames(coef_model), coef_model[main2, "Estimate"], 0)
      beta_interaction <- ifelse(interaction_term %in% rownames(coef_model),
                                 coef_model[interaction_term, "Estimate"], 0)

      log_odds <- beta_main2 + beta_interaction
      odds_ratio <- exp(log_odds)

      message(paste0("(i,j): ", i, j, ", beta_main1:", beta_main1, ", beta_main2:", beta_main2,
                     ", beta_interaction:", beta_interaction, ", log_odds:", log_odds))

      vcov_m <- vcov(model)

      sentences <- c(
        sentences,
        paste0(
          "The odds ratio of '", outcome, "' for the ",
          variable2," group ", j, " vs ",variable2," group ", ref_level2," (reference level) in the context of ",variable1, " group ", i," is ",signif(odds_ratio, sigfig), "."
        )
      )

      if(i==ref_level1){
        sentences <- c(
          sentences,
          paste0(
            "The 95% CI: (",signif(exp(log_odds + (-1.96) * sqrt(vcov_m[main2, main2])),sigfig), ", ", signif(exp(log_odds + 1.96 * sqrt(vcov_m[main2, main2])),sigfig),")"
          )
        )
      } else {
        sentences <- c(
          sentences,
          paste0(
            "The 95% CI:", paste0(
              "The 95% CI: (",signif(exp(log_odds + (-1.96) * sqrt(vcov_m[main2, main2] + vcov_m[interaction_term, interaction_term] + 2*vcov_m[main2,interaction_term])),sigfig),
              ", ", signif(exp(log_odds + 1.96 * sqrt(vcov_m[main2, main2] + vcov_m[interaction_term, interaction_term] + 2*vcov_m[main2,interaction_term])),sigfig),")"
            )
          )
        )
      }
    }
  }

  return(cat(sentences, sep = "\n"))
}
