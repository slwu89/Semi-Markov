###############################################################################
#      _____                _ __  ___      ____  __
#     / ___/___  ____ ___  (_)  |/  /___ _/ __ \/ /______ _   __
#     \__ \/ _ \/ __ `__ \/ / /|_/ / __ `/ /_/ / //_/ __ \ | / /
#    ___/ /  __/ / / / / / / /  / / /_/ / _, _/ ,< / /_/ / |/ /
#   /____/\___/_/ /_/ /_/_/_/  /_/\__,_/_/ |_/_/|_|\____/|___/
#
#   Original Semi-Markov C++ library by Andrew Dolgert (http://cbsu.tc.cornell.edu/staff/drew/)
#   R Package by Sean Wu (https://slwu89.github.io)
#   September 6, 2017
#
###############################################################################


###############################################################################
#   WeibullDistribution: Class Definition
###############################################################################

#' WeibullDistribution Class Definition
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
WeibullDistribution <- R6::R6Class(classname="WeibullDistribution",
                     portable = TRUE,
                     cloneable = TRUE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #################################################
                       # Constructor
                       #################################################

                       initialize = function(lambda, k, enabling_time, shift = 0.0, normal = 1.0){

                         private$lambda = lambda
                         private$k = k
                         private$enabling_time = enabling_time
                         private$shift = shift
                         private$normal = normal

                       }

                     ),

                     #private members
                     private = list(

                       # fields
                       lambda = numeric(1),
                       k = numeric(1),
                       enabling_time = numeric(1),
                       shift = numeric(1),
                       normal = numeric(1)

                     )

) #end class definition


###############################################################################
#   WeibullDistribution: Class Methods
###############################################################################

#' WeibullDistribution: Sample
#'
#' im a method!
#'  * This method is bound to \code{WeibullDistribution$Sample}
#'
#' @param current_time numeric
#'
Sample_WeibullDistribution <- function(current_time){

  U = runif(n=1,min=0,max=1) / private$normal
  if(U>=1){
    return(Inf)
  }

  d = current_time - (private$enabling_time + private$shift)
  value = numeric(1)
  if(d>0){
    value = private$lambda * (-log(1-U) + (d/private$lambda)^k)^(1/private$k) - d
  } else {
    value = -d + private$lambda * (-log(1-U))^(1/private$k)
  }

  if(value <= 0){
    stop(paste0("value ",value," less than or equal to 0"))
  }
  return(current_time + value)
}

WeibullDistribution$set(which = "public",name = "Sample",
  value = Sample_WeibullDistribution, overwrite = TRUE
)


#' WeibullDistribution: Bounded Hazard
#'
#' im a method!
#'  * This method is bound to \code{WeibullDistribution$BoundedHazard}
#'
BoundedHazard_WeibullDistribution <- function(){
  return(TRUE)
}

WeibullDistribution$set(which = "public",name = "BoundedHazard",
  value = BoundedHazard_WeibullDistribution, overwrite = TRUE
)


#' WeibullDistribution: Hazard Integral
#'
#' im a method!
#'  * This method is bound to \code{WeibullDistribution$HazardIntegral}
#'
#' @param t0 numeric
#' @param t1 numeric
#'
HazardIntegral_WeibullDistribution <- function(t0, t1){
  return(
    ((t1-private$enabling_time)/private$lambda)^private$k - ((t0-private$enabling_time)/private$lambda)^private$k
  )
}

WeibullDistribution$set(which = "public",name = "HazardIntegral",
  value = HazardIntegral_WeibullDistribution, overwrite = TRUE
)


#' WeibullDistribution: ImplicitHazardIntegral
#'
#' im a method!
#'  * This method is bound to \code{WeibullDistribution$ImplicitHazardIntegral}
#'
#' @param xa numeric
#' @param t0 numeric
#'
ImplicitHazardIntegral_WeibullDistribution <- function(xa, t0){
  return(
    private$enabling_time + private$lambda * (xa + ((t0-private$enabling_time)/private$lambda)^private$k)^(1/private$k)
  )
}

WeibullDistribution$set(which = "public",name = "ImplicitHazardIntegral",
  value = ImplicitHazardIntegral_WeibullDistribution, overwrite = TRUE
)


#' WeibullDistribution: Check Samples
#'
#' im a method!
#'  * This method is bound to \code{WeibullDistribution$CheckSamples}
#'
#' @param samples numeric vector
#' @param dt numeric
#'
CheckSamples_WeibullDistribution <- function(samples, dt){

  pass = TRUE

  if(abs(dt-0)<1e-6){

    # check empirical vs. theoretical mean
    expected_mean = private$lambda * gamma(1+1/private$k)
    mean = mean(samples)
    if(self$FracError(expected_mean,mean) > 1e-2){
      pass = FALSE
      print(paste0("Expected mean ",expected_mean," but found ",mean))
    }

    # check empirical vs. theoretical variance
    expected_variance = private$l * private$l * (
      gamma(1+2/private$k) - (gamma(1+1/private$k))^2
    )
    variance = var(samples)
    if(self$FracError(expected_variance,variance) > 1e-2){
      pass = FALSE
      print(paste0("Expected variance ",expected_variance," but found ",variance))
    }

    # check empirical vs. theoretical lambda
    min = min(samples)
    mink = min^private$k

    total = vapply(X = samples,FUN = function(x){x^private$k},FUN.VALUE = numeric(1),USE.NAMES = FALSE)
    total = sum(total)
    lambda_estimator = (total/length(samples) - mink)^(1/private$k)

    if(self$FracError(private$lambda, lambda_estimator) > 1e-2){
      pass = FALSE
      print(paste0("Expected lambda ",private$lambda," but found ",lambda_estimator))
    }

    # check empirical k vs. theoretical k
    kcheck = vapply(X = samples,FUN = function(x,mink,k,min){
      out = numeric(3)
      out[1] = (x^k) * log(x) - mink*log(min) -  # numerator
      out[2] = (x^k) - mink # denominator
      out[3] = log(x) # logsum
      return(out)
    },FUN.VALUE = numeric(3),USE.NAMES = FALSE,mink=mink,k=private$k,min=min)
    kcheck = rowSums(kcheck)

    k_est_inv = kcheck[1]/kcheck[2] - kcheck[3]/length(samples)
    k_est = 1/k_est_inv

    pass = self$CheckFracError(k,k_est,1e-2,"k")

  }

  return(pass)
}

WeibullDistribution$set(which = "public",name = "CheckSamples",
  value = CheckSamples_WeibullDistribution, overwrite = TRUE
)
