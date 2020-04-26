#include "RcppArmadillo.h"
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]

double Rsquared(double SSx,
                double SSy,
                double SSxy){
// SSxy/sqrt(SSxx SSyy)

  double r, r2;
  
  r = SSxy / pow(SSx * SSy, 0.5);
  
  r2 = pow(r, 2);
  
  return r2;
}
