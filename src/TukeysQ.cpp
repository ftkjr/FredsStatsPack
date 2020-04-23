#include "RcppArmadillo.h"
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]


double TukeysQ(double xi, double xj, double msw, double n) {
  // Where xi is the largest mean
  //       xj is the smallest mean
  double q = (xi - xj) / pow(msw / n, 0.5);
  return q;
}
