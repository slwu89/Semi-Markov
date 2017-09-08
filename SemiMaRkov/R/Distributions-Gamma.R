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

#' GammaDistribution: Set \code{alpha}
#'
#' im a method!
#'  * This method is bound to \code{GammaDistribution$get_alpha}
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

#' GammaDistribution: Set \code{beta}
#'
#' im a method!
#'  * This method is bound to \code{GammaDistribution$get_beta}
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

#' GammaDistribution: Set \code{enabling_time}
#'
#' im a method!
#'  * This method is bound to \code{GammaDistribution$get_enabling_time}
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
      (ga - tgamma_lower_boost(a=private$alpha,x=private$beta*(t0-private$enabling_time))) /
        (ga - tgamma_lower_boost(a=private$alpha,x=private$beta*(t1-private$enabling_time)))
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
  quad = 1 - exp(-xa)*(1-gamma_p_boost(private$alpha,private$beta*(t0-private$enabling_time)))
  return(
    private$enabling_time + gamma_p_inv_boost(private$alpha,quad)/private$beta
  )
}

GammaDistribution$set(which = "public",name = "ImplicitHazardIntegral",
  value = ImplicitHazardIntegral_GammaDistribution, overwrite = TRUE
)


#' GammaDistribution: Check Samples
#'
#' im a method!
#'  * This method is bound to \code{GammaDistribution$CheckSamples}
#'
#' @param samples numeric vector
#' @param dt numeric
#'
CheckSamples_GammaDistribution <- function(samples, dt){
  pass = TRUE



  return(pass)
}

GammaDistribution$set(which = "public",name = "CheckSamples",
  value = CheckSamples_GammaDistribution, overwrite = TRUE
)




# bool CheckSamples(const std::vector<double>& samples, double dt) {
#   namespace bac=boost::accumulators;
#   bool pass=true;
#   double a=std::get<0>(params_);
#   double th=1.0/std::get<1>(params_);
#
#   bac::accumulator_set<double, bac::stats<bac::tag::mean,
#       bac::tag::moment<2>, bac::tag::moment<3>,
#       bac::tag::variance(bac::lazy)>> acc;
#   for (auto st : samples) {
#     acc(st);
#   }
#
#   if (std::abs(dt-0)<0.0000001) {
#     double expected_mean=a*th;
#     double expected_variance=a*th*th;
#     double expected_skew=2/std::sqrt(a);
#     pass=detail::CheckFracError(
#         expected_mean, bac::mean(acc), 0.01, "mean");
#     pass=detail::CheckFracError(
#         expected_variance, bac::variance(acc), 0.01, "variance");
#     pass=detail::CheckFracError(
#         expected_skew, bac::moment<3>(acc), 0.01, "skew");
#   }
#
#   double th_est=bac::mean(acc)/a;
#   pass=detail::CheckFracError(th, th_est, 0.01, "theta");
#
#   // Following wikipedia on Gamma distribution...
#   double slog=0.0;
#   for (auto sl : samples) {
#     slog+=std::log(sl);
#   }
#   double s=std::log(bac::mean(acc))-slog/samples.size();
#   double a_est=(3-s+std::sqrt((s-3)*(s-3)+24*s))/(12*s);
#   pass=detail::CheckFracError(a, a_est, 0.03, "alpha");
#   return pass;
# }
# };
