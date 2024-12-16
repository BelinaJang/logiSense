#' @title Logistic Regression Interpretation for One Two-Way Interaction
#' @description a function that returns the interpretation for a logistic regression model with one two-way interaction.
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param variable1 name of the first variable in the interaction of interest
#' @param variable2 name of the second variable in the interaction of interest
#' @param variable1_type type of the first variable in the interaction of interest (either "continuous" or "categorical")
#' @param variable2_type type of the second variable in the interaction of interest (either "continuous" or "categorical")
#' @param data The name of the dataset
#' @param sigfig integer indicating the number of significant digits to be used.
#' @return `logint` prints the interpretation of interaction effects directly to the console using "cat()"
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
#' logint(formula, variable1, variable2, variable1_type, variable2_type, data, sigfig)
#' ## Example 2: Interaction between two continuous variables
#' formula <- stroke ~ age*avg_glucose_level
#' variable1 <- "age"
#' variable2 <- "avg_glucose_level"
#' variable1_type <- "continuous"
#' variable2_type <- "continuous"
#' data <- test_data
#' sigfig <- 4
#' logint(formula, variable1, variable2, variable1_type, variable2_type, data, sigfig)
#' ## Example 3: Interaction between two categorical variables
#' formula <- stroke ~ work_type*Residence_type
#' variable1 <- "work_type"
#' variable2 <- "Residence_type"
#' variable1_type <- "categorical"
#' variable2_type <- "categorical"
#' data <- test_data
#' sigfig <- 4
#' logint(formula, variable1, variable2, variable1_type, variable2_type, data, sigfig)
#' @export

logint <- function(formula, variable1, variable2, variable1_type, variable2_type, data, sigfig=4) {

  # check if sigfig is numeric
  if (!is.numeric(sigfig)){
    stop("sigfig should be numeric.")
  }

  #####################################################
  if (variable1_type == "continuous" & variable2_type == "continuous"){
    #both continuous <- don't really need this part
    continuous_var1 <- variable1
    continuous_var2 <- variable2
    message("Both variables are continuous. \n")
    message("continuous_var1: ", continuous_var1, ", continuous_var2: ", continuous_var2, "\n")

  } else if (variable1_type == "categorical" & variable2_type == "categorical"){
    #both categorical <- don't really need this part
    categorical_var1 <- variable1
    categorical_var2 <- variable2
    message("Both variables are categorical. \n")
    message("categorical_var1: ", categorical_var1, ", categorical_var2: ", categorical_var2, "\n")

  } else if (variable1_type != variable2_type){
    # when two variables are different types, assign each variable to appropriate type variable
    message("Both variables are of different types. \n")

    if (variable1_type == "continuous"){
      continuous_var <- variable1
      message("continuous_var: ", continuous_var)
    } else if (variable1_type == "categorical"){
      categorical_var <- variable1
      message("categorical_var: ", categorical_var)
    } else {
      stop("variable1_type should be either 'continuous' or 'categorical'.")
    }
    if (variable2_type == "continuous"){
      continuous_var <- variable2
      message(", continuous_var: ", continuous_var, "\n")
    } else if (variable2_type == "categorical"){
      categorical_var <- variable2
      message(", categorical_var: ", categorical_var, "\n")
    } else {
      stop("variable2_type should be either 'continuous' or 'categorical'.")
    }
  }
  #####################################################

  # run below if one of the types are continuous and the other one is categorical
  if ( (variable1_type=="continuous" & variable2_type=="categorical") | (variable1_type=="categorical" & variable2_type=="continuous")){
    logint_contbycat(formula, continuous_var, categorical_var, data, sigfig)
  } else if (variable1_type=="continuous" & variable2_type=="continuous"){
    # run below if both variables are continuous
    logint_contbycont(formula, variable1, variable2, data, sigfig)
  } else if (variable1_type=="categorical" & variable2_type=="categorical"){
    # run below if both variables are categorical
    logint_catbycat(formula, variable1, variable2, data, sigfig)
  }
}
