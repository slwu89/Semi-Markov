###############################################################################
#      _____                _ __  ___      ____  __
#     / ___/___  ____ ___  (_)  |/  /___ _/ __ \/ /______ _   __
#     \__ \/ _ \/ __ `__ \/ / /|_/ / __ `/ /_/ / //_/ __ \ | / /
#    ___/ /  __/ / / / / / / /  / / /_/ / _, _/ ,< / /_/ / |/ /
#   /____/\___/_/ /_/ /_/_/_/  /_/\__,_/_/ |_/_/|_|\____/|___/
#
#   Original Semi-Markov C++ library by Andrew Dolgert (http://cbsu.tc.cornell.edu/staff/drew/)
#   R Package by Sean Wu (https://slwu89.github.io)
#   September 5, 2017
#
###############################################################################


#' Fractional Error
#'
#' Used to in \code{\link{CheckFracError}}
#'  * This method is bound to \code{Distribution$FracError} for:
#'    * \code{\link{ExponentialDistribution}}
#'    * \code{\link{ShiftedExponentialDistribution}}
#'
#'
#' @param a numeric
#' @param b numeric
#'
FracError <- function(a,b){
  return(abs((a-b)/a))
}

ExponentialDistribution$set(which = "public",name = "FracError",
  value = FracError, overwrite = TRUE
)

ShiftedExponentialDistribution$set(which = "public",name = "FracError",
  value = FracError, overwrite = TRUE
)


#' Check Fractional Error
#'
#' If the fractional error $\frac{\left | a-b \right |}{b} > tol$ return \code{FALSE} and alert the user, otherwise return \code{TRUE}
#'  * This method is bound to \code{Distribution$CheckFracError} for
#'    * \code{\link{ExponentialDistribution}}
#'    * \code{\link{ShiftedExponentialDistribution}}
#'
#' @param a numeric
#' @param b numeric
#' @param tol numeric
#' @param m character
#'
CheckFracError <- function(a,b,tol,m){
  if(self$FracError(a,b)>tol){
    print(paste0("Fractional error of ",m," too large. Expected ",a," found ",b))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

ExponentialDistribution$set(which = "public",name = "CheckFracError",
  value = CheckFracError, overwrite = TRUE
)

ShiftedExponentialDistribution$set(which = "public",name = "CheckFracError",
  value = CheckFracError, overwrite = TRUE
)
