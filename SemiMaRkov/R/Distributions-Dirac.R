###############################################################################
#      _____                _ __  ___      ____  __
#     / ___/___  ____ ___  (_)  |/  /___ _/ __ \/ /______ _   __
#     \__ \/ _ \/ __ `__ \/ / /|_/ / __ `/ /_/ / //_/ __ \ | / /
#    ___/ /  __/ / / / / / / /  / / /_/ / _, _/ ,< / /_/ / |/ /
#   /____/\___/_/ /_/ /_/_/_/  /_/\__,_/_/ |_/_/|_|\____/|___/
#
#   Original Semi-Markov C++ library by Andrew Dolgert (http://cbsu.tc.cornell.edu/staff/drew/)
#   R Package by Sean Wu (https://slwu89.github.io)
#   DiracDistribution
#   September 8, 2017
#
###############################################################################


###############################################################################
#   DiracDistribution: Class Definition
###############################################################################

#' DiracDistribution Class Definition
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
DiracDistribution <- R6::R6Class(classname="DiracDistribution",
                     portable = TRUE,
                     cloneable = TRUE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #################################################
                       # Constructor
                       #################################################

                       initialize = function(value, enabling_time){

                         private$value = value
                         private$enabling_time = enabling_time

                       }

                     ),

                     #private members
                     private = list(

                       # fields
                       value = numeric(1),
                       enabling_time = numeric(1)

                     )

) #end class definition

# Getters & Setters

#' DiracDistribution: Get \code{alpha}
#'
#' im a method!
#'  * This method is bound to \code{DiracDistribution$get_value}
#'
get_value_DiracDistribution <- function(){
  return(private$value)
}

DiracDistribution$set(which = "public",name = "get_alpha",
  value = get_value_DiracDistribution, overwrite = TRUE
)

#' DiracDistribution: Set \code{alpha}
#'
#' im a method!
#'  * This method is bound to \code{DiracDistribution$get_alpha}
#'
#' @param value numeric
#'
set_value_DiracDistribution <- function(value){
  private$value = value
}

DiracDistribution$set(which = "public",name = "set_value",
  value = set_value_DiracDistribution, overwrite = TRUE
)

#' DiracDistribution: Get \code{enabling_time}
#'
#' im a method!
#'  * This method is bound to \code{DiracDistribution$get_enabling_time}
#'
get_enabling_time_DiracDistribution <- function(){
  return(private$enabling_time)
}

DiracDistribution$set(which = "public",name = "get_enabling_time",
  value = get_enabling_time_DiracDistribution, overwrite = TRUE
)

#' DiracDistribution: Set \code{enabling_time}
#'
#' im a method!
#'  * This method is bound to \code{DiracDistributio$get_enabling_time}
#'
#' @param enabling_time numeric
#'
set_enabling_time_DiracDistribution <- function(enabling_time){
  private$enabling_time = enabling_time
}

DiracDistribution$set(which = "public",name = "set_enabling_time",
  value = set_enabling_time_DiracDistribution, overwrite = TRUE
)


###############################################################################
#   DiracDistribution: Class Methods
###############################################################################

#' DiracDistribution: Sample
#'
#' im a method!
#'  * This method is bound to \code{DiracDistribution$Sample}
#'
#' @param current_time numeric
#'
Sample_DiracDistribution <- function(current_time){
  return(private$value)
}

DiracDistribution$set(which = "public",name = "Sample",
  value = Sample_DiracDistribution, overwrite = TRUE
)
