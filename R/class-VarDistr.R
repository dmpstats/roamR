#' Class `<VarDistr>`
#'
#'
#' `<VarDistr>` is an S4 class that encapsulates the distributional properties
#' of a variable of interest. It defines the variable in terms of its mean
#' and coefficient of variation (CV). Intended to provide a structured way to specify
#' input values and quantify the uncertainty associated with their estimates.
#'
#' @slot mean numeric, the expected value of the variable.
#' @slot cv numeric, the coefficient of variation (CV), representing the
#'   relative uncertainty of the variable as a proportion of its expected value

methods::setClass(
  Class = "VarDistr",
  slots = list(
    mean = "numeric",
    cv = "numeric"
  ),
  prototype = list(
    mean = NA_real_,
    cv = NA_real_
  )
)






VarDistr <- function(mean = NA, cv = NA){

  if(is.na(mean)) mean <- NA_real_
  if(is.na(cv)) cv <- NA_real_

  new("VarDistr", mean = mean, cv = cv)
}





