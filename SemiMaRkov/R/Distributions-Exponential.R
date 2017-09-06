###############################################################################
#      _____                _ __  ___      ____  __
#     / ___/___  ____ ___  (_)  |/  /___ _/ __ \/ /______ _   __
#     \__ \/ _ \/ __ `__ \/ / /|_/ / __ `/ /_/ / //_/ __ \ | / /
#    ___/ /  __/ / / / / / / /  / / /_/ / _, _/ ,< / /_/ / |/ /
#   /____/\___/_/ /_/ /_/_/_/  /_/\__,_/_/ |_/_/|_|\____/|___/
#
#   Original Semi-Markov C++ library by Andrew Dolgert (http://cbsu.tc.cornell.edu/staff/drew/)
#   R Package by Sean Wu (https://slwu89.github.io)
#   ExponentialDistribution & ShiftedExponentialDistribution
#   September 5, 2017
#
###############################################################################


###############################################################################
#   ExponentialDistribution: Class Definition
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

                       initialize = function(lambda, enabling_time, normal = 1.0){

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

# Getters & Setters

#' ExponentialDistribution: Get \code{enabling_time}
#'
#' im a method!
#'  * This method is bound to \code{ExponentialDistribution$get_enabling_time}
#'
get_enabling_time_ExponentialDistribution <- function(){
  return(private$enabling_time)
}

ExponentialDistribution$set(which = "public",name = "get_enabling_time",
  value = get_enabling_time_ExponentialDistribution, overwrite = TRUE
)

#' ExponentialDistribution: Set \code{enabling_time}
#'
#' im a method!
#'  * This method is bound to \code{ExponentialDistribution$get_enabling_time}
#'
#' @param enabling_time numeric
#'
set_enabling_time_ExponentialDistribution <- function(enabling_time){
  private$enabling_time = enabling_time
}

ExponentialDistribution$set(which = "public",name = "set_enabling_time",
  value = set_enabling_time_ExponentialDistribution, overwrite = TRUE
)

#' ExponentialDistribution: Get \code{lambda}
#'
#' im a method!
#'  * This method is bound to \code{ExponentialDistribution$get_lambda}
#'
get_lambda_ExponentialDistribution <- function(){
  return(private$lambda)
}

ExponentialDistribution$set(which = "public",name = "get_lambda",
  value = get_lambda_ExponentialDistribution, overwrite = TRUE
)

#' ExponentialDistribution: Set \code{lambda}
#'
#' im a method!
#'  * This method is bound to \code{ExponentialDistribution$set_lambda}
#'
#' @param lambda numeric
#'
set_lambda_ExponentialDistribution <- function(lambda){
  private$lambda = lambda
}

ExponentialDistribution$set(which = "public",name = "set_lambda",
  value = set_lambda_ExponentialDistribution, overwrite = TRUE
)

#' ExponentialDistribution: Get \code{normal}
#'
#' im a method!
#'  * This method is bound to \code{ExponentialDistribution$get_normal}
#'
get_normal_ExponentialDistribution <- function(){
  return(private$enabling_time)
}

ExponentialDistribution$set(which = "public",name = "get_normal",
  value = get_normal_ExponentialDistribution, overwrite = TRUE
)

#' ExponentialDistribution: Set \code{normal}
#'
#' im a method!
#'  * This method is bound to \code{ExponentialDistribution$set_normal}
#'
#' @param lambda numeric
#'
set_normal_ExponentialDistribution <- function(normal){
  private$normal = normal
}

ExponentialDistribution$set(which = "public",name = "set_normal",
  value = set_normal_ExponentialDistribution, overwrite = TRUE
)


###############################################################################
#   ExponentialDistribution: Class Methods
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
  if(U>=1){
    return(Inf)
  } else {
    return(current_time - log(U)/private$lambda)
  }
}

ExponentialDistribution$set(which = "public",name = "Sample",
  value = Sample_ExponentialDistribution, overwrite = TRUE
)


#' ExponentialDistribution: Bounded Hazard
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


#' ExponentialDistribution: Hazard Integral
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


#' ExponentialDistribution: Implicit Hazard Integral
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


#' ExponentialDistribution: Check Samples
#'
#' im a method!
#'  * This method is bound to \code{ExponentialDistribution$CheckSamples}
#'
#' @param samples numeric vector
#' @param dt numeric
#'
CheckSamples_ExponentialDistribution <- function(samples, dt){

  pass = TRUE
  lambda_estimator = 1 / mean(samples)
  too_low = private$lambda < lambda_estimator*(1-1.96/sqrt(length(samples)))
  too_high = private$lambda > lambda_estimator*(1+1.96/sqrt(length(samples)))

  if(too_low | too_high){
    print(paste0("Parameter not in bounds. Low? ",too_low," high? ",too_high))
    pass = FALSE
  }

  variance = var(samples)

  pass = self$CheckFracError(a = variance, b = private$lambda^2, tol = 0.01, m = "variance")
  return(pass)

}

ExponentialDistribution$set(which = "public",name = "CheckSamples",
  value = CheckSamples_ExponentialDistribution, overwrite = TRUE
)


###############################################################################
#   ShiftedExponentialDistribution: Class Definition
###############################################################################

#' ShiftedExponentialDistribution Class Definition
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
ShiftedExponentialDistribution <- R6::R6Class(classname="ShiftedExponentialDistribution",
                     portable = TRUE,
                     cloneable = TRUE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #################################################
                       # Constructor
                       #################################################

                       initialize = function(lambda, enabling_time, shift = 0.0, normal = 1.0){

                         if(!(lambda > 0)){
                           stop(paste0("lambda ",lambda," must be greater than 0"))
                         }

                         private$lambda = lambda
                         private$enabling_time = enabling_time
                         private$shift = shift
                         private$normal = normal

                       }

                     ),

                     #private members
                     private = list(

                       # fields
                       lambda = numeric(1),
                       enabling_time = numeric(1),
                       shift = numeric(1),
                       normal = numeric(1)

                     )

) #end class definition

# Getters & Setters

#' ShiftedExponentialDistribution: Get \code{enabling_time}
#'
#' im a method!
#'  * This method is bound to \code{ShiftedExponentialDistribution$get_enabling_time}
#'
get_enabling_time_ShiftedExponentialDistribution <- function(){
  return(private$enabling_time)
}

ShiftedExponentialDistribution$set(which = "public",name = "get_enabling_time",
  value = get_enabling_time_ShiftedExponentialDistribution, overwrite = TRUE
)

#' ShiftedExponentialDistribution: Set \code{enabling_time}
#'
#' im a method!
#'  * This method is bound to \code{ShiftedExponentialDistribution$get_enabling_time}
#'
#' @param enabling_time numeric
#'
set_enabling_time_ShiftedExponentialDistribution <- function(enabling_time){
  private$enabling_time = enabling_time
}

ShiftedExponentialDistribution$set(which = "public",name = "set_enabling_time",
  value = set_enabling_time_ShiftedExponentialDistribution, overwrite = TRUE
)

#' ShiftedExponentialDistribution: Get \code{lambda}
#'
#' im a method!
#'  * This method is bound to \code{ShiftedExponentialDistribution$get_lambda}
#'
get_lambda_ShiftedExponentialDistribution <- function(){
  return(private$lambda)
}

ShiftedExponentialDistribution$set(which = "public",name = "get_lambda",
  value = get_lambda_ShiftedExponentialDistribution, overwrite = TRUE
)

#' ShiftedExponentialDistribution: Set \code{lambda}
#'
#' im a method!
#'  * This method is bound to \code{ShiftedExponentialDistribution$set_lambda}
#'
#' @param lambda numeric
#'
set_lambda_ShiftedExponentialDistribution <- function(lambda){
  private$lambda = lambda
}

ShiftedExponentialDistribution$set(which = "public",name = "set_lambda",
  value = set_lambda_ShiftedExponentialDistribution, overwrite = TRUE
)

#' ShiftedExponentialDistribution: Get \code{shift}
#'
#' im a method!
#'  * This method is bound to \code{ShiftedExponentialDistribution$get_shift}
#'
get_shift_ShiftedExponentialDistribution <- function(){
  return(private$enabling_time)
}

ShiftedExponentialDistribution$set(which = "public",name = "get_shift",
  value = get_shift_ShiftedExponentialDistribution, overwrite = TRUE
)

#' ShiftedExponentialDistribution: Set \code{shift}
#'
#' im a method!
#'  * This method is bound to \code{ShiftedExponentialDistribution$set_shift}
#'
#' @param lambda numeric
#'
set_shift_ShiftedExponentialDistribution <- function(shift){
  private$shift = shift
}

ShiftedExponentialDistribution$set(which = "public",name = "set_shift",
  value = set_shift_ShiftedExponentialDistribution, overwrite = TRUE
)

#' ShiftedExponentialDistribution: Get \code{normal}
#'
#' im a method!
#'  * This method is bound to \code{ShiftedExponentialDistribution$get_normal}
#'
get_normal_ShiftedExponentialDistribution <- function(){
  return(private$enabling_time)
}

ShiftedExponentialDistribution$set(which = "public",name = "get_normal",
  value = get_normal_ShiftedExponentialDistribution, overwrite = TRUE
)

#' ShiftedExponentialDistribution: Set \code{normal}
#'
#' im a method!
#'  * This method is bound to \code{ShiftedExponentialDistribution$set_normal}
#'
#' @param lambda numeric
#'
set_normal_ShiftedExponentialDistribution <- function(normal){
  private$normal = normal
}

ShiftedExponentialDistribution$set(which = "public",name = "set_normal",
  value = set_normal_ShiftedExponentialDistribution, overwrite = TRUE
)


###############################################################################
#   ShiftedExponentialDistribution: Class Methods
###############################################################################


#' ShiftedExponentialDistribution: Sample
#'
#' im a method!
#'  * This method is bound to \code{ShiftedExponentialDistribution$Sample}
#'
#' @param current_time numeric
#'
Sample_ShiftedExponentialDistribution <- function(current_time){
  U = runif(n=1,min=0,max=1) / private$normal
  if(U>=1){
    return(Inf)
  } else if(current_time > private$enabling_time + private$shift){
    return(-log(U)/private$lambda)
  } else {
    return(
      (private$enabling_time + private$shift) - log(U)/private$normal - current_time
    )
  }
}

ShiftedExponentialDistribution$set(which = "public",name = "Sample",
  value = Sample_ShiftedExponentialDistribution, overwrite = TRUE
)


#' ShiftedExponentialDistribution: Bounded Hazard
#'
#' im a method!
#'  * This method is bound to \code{ShiftedExponentialDistribution$BoundedHazard}
#'
BoundedHazard_ShiftedExponentialDistribution <- function(){
  return(TRUE)
}

ShiftedExponentialDistribution$set(which = "public",name = "BoundedHazard",
  value = BoundedHazard_ShiftedExponentialDistribution, overwrite = TRUE
)


#' ShiftedExponentialDistribution: Implicit Hazard Integral
#'
#' This implicitly solves for a quantile by integrating the hazard. The function returns \eqn{t} in \deqn{x_{a}=\int_{t0}^{t}\lambda (s)ds}
#'  * This method is bound to \code{ShiftedExponentialDistribution$ImplicitHazardIntegral}
#'
#' @param xa numeric
#' @param t0 numeric
#'
ImplicitHazardIntegral_ShiftedExponentialDistribution <- function(xa, t0){
  start = max(t0,private$shift+private$enabling_time)
  return(start + xa/private$lambda)
}

ShiftedExponentialDistribution$set(which = "public",name = "HazardIntegral",
  value = ImplicitHazardIntegral_ShiftedExponentialDistribution, overwrite = TRUE
)


#' ShiftedExponentialDistribution: Check Samples
#'
#' im a method!
#'  * This method is bound to \code{ShiftedExponentialDistribution$CheckSamples}
#'
#' @param samples numeric vector
#' @param dt numeric
#'
CheckSamples_ShiftedExponentialDistribution <- function(samples, dt){

  pass = TRUE
  lambda_estimator = 1 / mean(samples)
  too_low = private$lambda < lambda_estimator*(1-1.96/sqrt(length(samples)))
  too_high = private$lambda > lambda_estimator*(1+1.96/sqrt(length(samples)))

  if(too_low | too_high){
    print(paste0("Parameter not in bounds. Low? ",too_low," high? ",too_high))
    pass = FALSE
  }

  variance = var(samples)

  pass = self$CheckFracError(a = variance, b = private$lambda^2, tol = 0.01, m = "variance")
  return(pass)

}

ShiftedExponentialDistribution$set(which = "public",name = "CheckSamples",
  value = CheckSamples_ShiftedExponentialDistribution, overwrite = TRUE
)
