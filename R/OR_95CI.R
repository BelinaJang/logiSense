#' @title odds ratio function
#' @description a function that returns the odds ratio and confidence intervals
#' @param coef vector of coefficient estimates
#' @param se vector of standard errors
#' @param siglevel significance level
#' @param roundto number of decimal places
#' @return "OR (ORlcl, ORucl)"
#' @importFrom stats qnorm
#' @examples
#' coef <- c(0.6262892, 0.1045653, -0.7847091)
#' se <- c(-1.253949, -1.245193, 0.423182)
#' siglevel <- 0.05
#' roundto <- 3
#' OR_95CI(coef, se, siglevel, roundto)
#' @export

OR_95CI <- function(coef, se, siglevel, roundto){
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall = roundto),
                     " (",
                     format(round(ORlcl, roundto), nsmall = roundto),
                     ", ",
                     format(round(ORucl, roundto), nsmall = roundto),
                     ")"
  )
  return(ORresult)
}
