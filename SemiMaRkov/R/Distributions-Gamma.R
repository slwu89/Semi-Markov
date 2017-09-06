###############################################################################
#      _____                _ __  ___      ____  __
#     / ___/___  ____ ___  (_)  |/  /___ _/ __ \/ /______ _   __
#     \__ \/ _ \/ __ `__ \/ / /|_/ / __ `/ /_/ / //_/ __ \ | / /
#    ___/ /  __/ / / / / / / /  / / /_/ / _, _/ ,< / /_/ / |/ /
#   /____/\___/_/ /_/ /_/_/_/  /_/\__,_/_/ |_/_/|_|\____/|___/
#
#   Original Semi-Markov C++ library by Andrew Dolgert (http://cbsu.tc.cornell.edu/staff/drew/)
#   R Package by Sean Wu (https://slwu89.github.io)
#   GammaDistribution
#   September 6, 2017
#
###############################################################################


###############################################################################
#   GammaDistribution: Class Definition
###############################################################################

#' GammaDistribution Class Definition
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
GammaDistribution <- R6::R6Class(classname="GammaDistribution",
                     portable = TRUE,
                     cloneable = TRUE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #################################################
                       # Constructor
                       #################################################

                       initialize = function(alpha, beta, enabling_time){

                         private$alpha = alpha
                         private$beta = beta
                         private$enabling_time = enabling_time

                       }

                     ),

                     #private members
                     private = list(

                       # fields
                       alpha = numeric(1),
                       beta = numeric(1),
                       enabling_time = numeric(1)

                     )

) #end class definition

# Getters & Setters

#' GammaDistribution: Get \code{alpha}
#'
#' im a method!
#'  * This method is bound to \code{GammaDistribution$get_alpha}
#'
get_alpha_GammaDistribution <- function(){
  return(private$alpha)
}

GammaDistribution$set(which = "public",name = "get_alpha",
  value = get_alpha_GammaDistribution, overwrite = TRUE
)

#' WeibullDistribution: Set \code{alpha}
#'
#' im a method!
#'  * This method is bound to \code{WeibullDistribution$get_alpha}
#'
#' @param alpha numeric
#'
set_alpha_GammaDistribution <- function(alpha){
  private$alpha = alpha
}

GammaDistribution$set(which = "public",name = "set_alpha",
  value = set_alpha_GammaDistribution, overwrite = TRUE
)

#' GammaDistribution: Get \code{beta}
#'
#' im a method!
#'  * This method is bound to \code{GammaDistribution$get_beta}
#'
get_beta_GammaDistribution <- function(){
  return(private$beta)
}

GammaDistribution$set(which = "public",name = "get_beta",
  value = get_beta_GammaDistribution, overwrite = TRUE
)

#' WeibullDistribution: Set \code{beta}
#'
#' im a method!
#'  * This method is bound to \code{WeibullDistribution$get_beta}
#'
#' @param beta numeric
#'
set_beta_GammaDistribution <- function(beta){
  private$beta = beta
}

GammaDistribution$set(which = "public",name = "set_beta",
  value = set_beta_GammaDistribution, overwrite = TRUE
)

#' GammaDistribution: Get \code{enabling_time}
#'
#' im a method!
#'  * This method is bound to \code{GammaDistribution$get_enabling_time}
#'
get_enabling_time_GammaDistribution <- function(){
  return(private$enabling_time)
}

GammaDistribution$set(which = "public",name = "get_enabling_time",
  value = get_enabling_time_GammaDistribution, overwrite = TRUE
)

#' WeibullDistribution: Set \code{enabling_time}
#'
#' im a method!
#'  * This method is bound to \code{WeibullDistribution$get_enabling_time}
#'
#' @param enabling_time numeric
#'
set_enabling_time_GammaDistribution <- function(enabling_time){
  private$enabling_time = enabling_time
}

GammaDistribution$set(which = "public",name = "set_enabling_time",
  value = set_enabling_time_GammaDistribution, overwrite = TRUE
)


###############################################################################
#   GammaDistribution: Class Methods
###############################################################################

#' GammaDistribution: Sample
#'
#' im a method!
#'  * This method is bound to \code{GammaDistribution$Sample}
#'
#' @param current_time numeric
#'
Sample_GammaDistribution <- function(current_time){

  U = runif(n=1,min=0,max=1)
  d = current_time - private$enabling_time

  if(d>0){
    cumulative = pgamma(q = d,shape = private$alpha,rate = private$beta)
    return(
      qgamma(p = U*(1-cumulative) + cumulative,shape = private$alpha,rate = private$beta) - d
    )
  } else {
    return(
      qgamma(p = U,shape = private$alpha,rate = private$beta) - d
    )
  }

}

GammaDistribution$set(which = "public",name = "Sample",
  value = Sample_GammaDistribution, overwrite = TRUE
)


#' GammaDistribution: Bounded Hazard
#'
#' im a method!
#'  * This method is bound to \code{GammaDistribution$BoundedHazard}
#'
BoundedHazard_GammaDistribution <- function(){
  return(TRUE)
}

GammaDistribution$set(which = "public",name = "BoundedHazard",
  value = BoundedHazard_GammaDistribution, overwrite = TRUE
)


#' GammaDistribution: Hazard Integral
#'
#' im a method!
#'  * This method is bound to \code{GammaDistribution$HazardIntegral}
#'
#' @param t0 numeric
#' @param t1 numeric
#'
HazardIntegral_GammaDistribution <- function(t0, t1){
  ga = gamma(private$alpha)
  return(
    log(
      (ga - zipfR::Igamma(a=private$alpha,x=private$beta*(t0-private$enabling_time))) /
        (ga - zipfR::Igamma(a=private$alpha,x=private$beta*(t1-private$enabling_time)))
    )
  )
}

GammaDistribution$set(which = "public",name = "HazardIntegral",
  value = HazardIntegral_GammaDistribution, overwrite = TRUE
)


#' GammaDistribution: Implicit Hazard Integral
#'
#' im a method!
#'  * This method is bound to \code{GammaDistribution$ImplicitHazardIntegral}
#'
#' @param xa numeric
#' @param t0 numeric
#'
ImplicitHazardIntegral_GammaDistribution <- function(xa, t0){
  # a=alpha
  # b = beta
  # te = enabling_time
  # quad = 1 - exp(-xa)*(1-boost::math::gamma_p(a, b*(t0-te)));
  return(NULL)
}

GammaDistribution$set(which = "public",name = "ImplicitHazardIntegral",
  value = ImplicitHazardIntegral_GammaDistribution, overwrite = TRUE
)
