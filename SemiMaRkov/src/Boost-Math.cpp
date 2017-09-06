//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//      _____                _ __  ___      ____  __
//     / ___/___  ____ ___  (_)  |/  /___ _/ __ \/ /______ _   __
//     \__ \/ _ \/ __ `__ \/ / /|_/ / __ `/ /_/ / //_/ __ \ | / /
//    ___/ /  __/ / / / / / / /  / / /_/ / _, _/ ,< / /_/ / |/ /
//   /____/\___/_/ /_/ /_/_/_/  /_/\__,_/_/ |_/_/|_|\____/|___/
//
//   Original Semi-Markov C++ library by Andrew Dolgert (http://cbsu.tc.cornell.edu/staff/drew/)
//   R Package by Sean Wu (https://slwu89.github.io)
//   Wrappers to Boost Math Functions
//   September 5, 2017
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include <Rcpp.h>
// [[Rcpp::depends(BH)]]

#include <boost/math/special_functions/gamma.hpp>

using namespace Rcpp;

//' Full (non-normalised) Upper Incomplete Gamma Function
//'
//' Return the full (non-normalised) upper incomplete gamma function; this is a wrapper for
//' \code{tgamma}, found in \url{http://www.boost.org/doc/libs/1_46_0/libs/math/doc/sf_and_dist/html/math_toolkit/special/sf_gamma/igamma.html}
//'
//' @param a numeric
//' @param z numeric
//'
//' @export
// [[Rcpp::export]]
double tgamma_boost(const double& a, const double& z){
  double out = boost::math::tgamma(a,z);
  return(out);
}

//' Full (non-normalised) Lower Incomplete Gamma Function
//'
//' Return the full (non-normalised) lower incomplete gamma function; this is a wrapper for
//' \code{tgamma_lower}, found in \url{http://www.boost.org/doc/libs/1_46_0/libs/math/doc/sf_and_dist/html/math_toolkit/special/sf_gamma/igamma.html}
//'
//' @param a numeric
//' @param z numeric
//'
//' @export
// [[Rcpp::export]]
double tgamma_lower_boost(const double& a, const double& z){
  double out = boost::math::tgamma_lower(a,z);
  return(out);
}

//' Normalised Lower Incomplete Gamma Function
//'
//' Return the normalised lower incomplete gamma function; this is a wrapper for
//' \code{gamma_p}, found in \url{http://www.boost.org/doc/libs/1_46_0/libs/math/doc/sf_and_dist/html/math_toolkit/special/sf_gamma/igamma.html}
//'
//' @param a numeric
//' @param z numeric
//'
//' @export
// [[Rcpp::export]]
double gamma_p_boost(const double& a, const double& z){
  double out = boost::math::gamma_p(a,z);
  return(out);
}

//' Normalised Upper Incomplete Gamma Function
//'
//' Return the normalised upper incomplete gamma function; this is a wrapper for
//' \code{gamma_p_inv}, found in \url{http://www.boost.org/doc/libs/1_46_0/libs/math/doc/sf_and_dist/html/math_toolkit/special/sf_gamma/igamma.html}
//'
//' @param a numeric
//' @param z numeric
//'
//' @export
// [[Rcpp::export]]
double gamma_p_inv_boost(const double& a, const double& z){
  double out = boost::math::gamma_p_inv(a,z);
  return(out);
}
