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
#'
#' @param a numeric
#' @param b numeric
#'
#' @export
FracError <- function(a,b){
  return(abs((a-b)/a))
}

#' Check Fractional Error
#'
#' Return logical value
#'
#' @param a numeric
#' @param b numeric
#'
#' @export
CheckFracError <- function(a,b,tol,m){
  if(FracError(a,b)>tol){
    print(paste0("Fractional error of ",m," too large. Expected ",a," found ",b))
    return(FALSE)
  } else {
    return(TRUE)
  }
}


###############################################################################
#   Class Definition
###############################################################################

#' ExponentialDistribution Class Definition
#'
#' do something
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: im an agument!
#'
#' @section **Methods**:
#'  * method: im a method!
#'
#' @section **Fields**:
#'  * field: im a field!
#'
#' @export
ExponentialDistribution <- R6::R6Class(classname="ExponentialDistribution",
                     portable = TRUE,
                     cloneable = TRUE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #################################################
                       # Constructor
                       #################################################

                       initialize = function(lambda, enabling_time, normal){

                         if(!(lambda > 0)){
                           stop(paste0("lambda ",lambda," must be greater than 0"))
                         }

                         private$lambda = lambda
                         private$enabling_time = enabling_time
                         private$normal = normal

                       }

                     ),

                     #private members
                     private = list(

                       # fields
                       lambda = numeric(1),
                       enabling_time = numeric(1),
                       normal = numeric(1)

                     )

) #end class definition


###############################################################################
#   Class Methods
###############################################################################

#' ExponentialDistribution: Sample
#'
#' im a method!
#'  * This method is bound to \code{ExponentialDistribution$Sample}
#'
#' @param current_time numeric
#'
Sample_ExponentialDistribution <- function(current_time){
  U = runif(n=1,min=0,max=1) / private$normal
  if(U>1){
    return(Inf)
  } else {
    return(current_time - log(U)/private$lambda)
  }
}

ExponentialDistribution$set(which = "public",name = "Sample",
  value = Sample_ExponentialDistribution, overwrite = TRUE
)


#' ExponentialDistribution: BoundedHazard
#'
#' im a method!
#'  * This method is bound to \code{ExponentialDistribution$BoundedHazard}
#'
BoundedHazard_ExponentialDistribution <- function(){
  return(TRUE)
}

ExponentialDistribution$set(which = "public",name = "BoundedHazard",
  value = BoundedHazard_ExponentialDistribution, overwrite = TRUE
)


#' ExponentialDistribution: HazardIntegral
#'
#' im a method!
#'  * This method is bound to \code{ExponentialDistribution$HazardIntegral}
#'
#' @param t0 numeric
#' @param t1 numeric
#'
HazardIntegral_ExponentialDistribution <- function(current_time){
  return(private$lambda * (t1-t0))
}

ExponentialDistribution$set(which = "public",name = "HazardIntegral",
  value = HazardIntegral_ExponentialDistribution, overwrite = TRUE
)


#' ExponentialDistribution: ImplicitHazardIntegral
#'
#' im a method!
#'  * This method is bound to \code{ExponentialDistribution$ImplicitHazardIntegral}
#'
#' @param xa numeric
#' @param t0 numeric
#'
ImplicitHazardIntegral_ExponentialDistribution <- function(current_time){
  return((t0+xa)/private$lambda)
}

ExponentialDistribution$set(which = "public",name = "ImplicitHazardIntegral",
  value = ImplicitHazardIntegral_ExponentialDistribution, overwrite = TRUE
)


#' ExponentialDistribution: EnablingTime
#'
#' im a method!
#'  * This method is bound to \code{ExponentialDistribution$EnablingTime}
#'
EnablingTime_ExponentialDistribution <- function(current_time){
  return(private$enabling_time)
}

ExponentialDistribution$set(which = "public",name = "EnablingTime",
  value = EnablingTime_ExponentialDistribution, overwrite = TRUE
)


#' ExponentialDistribution: CheckSamples
#'
#' im a method!
#'  * This method is bound to \code{ExponentialDistribution$CheckSamples}
#'
#' @param samples numeric vector
#' @param dt numeric
#'
CheckSamples_ExponentialDistribution <- function(samples, dt){

  pass = logical(1)
  lambda_estimator = 1 / mean(samples)
  too_low = private$lambda < lambda_estimator*(1-1.96/sqrt(length(samples)))
  too_high = private$lambda > lambda_estimator*(1+1.96/sqrt(length(samples)))

  if(too_low | too_high){
    print(paste0("Parameter not in bounds. Low? ",too_low," high? ",too_high))
    pass = FALSE
  }

  variance = var(samples)



}

ExponentialDistribution$set(which = "public",name = "CheckSamples",
  value = CheckSamples_ExponentialDistribution, overwrite = TRUE
)
