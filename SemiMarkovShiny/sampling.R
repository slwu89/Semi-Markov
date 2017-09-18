###############################################################################
#      _____                _ __  ___      ____  __
#     / ___/___  ____ ___  (_)  |/  /___ _/ __ \/ /______ _   __
#     \__ \/ _ \/ __ `__ \/ / /|_/ / __ `/ /_/ / //_/ __ \ | / /
#    ___/ /  __/ / / / / / / /  / / /_/ / _, _/ ,< / /_/ / |/ /
#   /____/\___/_/ /_/ /_/_/_/  /_/\__,_/_/ |_/_/|_|\____/|___/
#
#   Original Semi-Markov C++ library by Andrew Dolgert (http://cbsu.tc.cornell.edu/staff/drew/)
#   Shiny App by Sean Wu (https://slwu89.github.io)
#   September 17, 2017
#
###############################################################################

sampler <- function(t,te,t0,dist){
  # sanity checks
  if(!is.character(dist) | !dist %in% c("gamma","exp","weibull")){
    stop(cat("dist: expected character in 'gamma', 'exp', 'weibull', got: ",dist,"\n",sep=""))
  }
  if(!any(is.numeric(c(t,te,t0)))){
   stop(cat("one of t, te, t0 not numeric, please enter only numeric values\n",sep=""))
  }
  tSeq = seq()
  switch(dist,
    gamma = {
      
    },
    exp = {
      
    },
    weibull = {
      
    }
  ) 
  
}