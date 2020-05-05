#include "RcppArmadillo.h"
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]

double PhiSquared_D(int a,
                    int n,
                    double D,
                    double sigma){
  
  // $\Phi ^ 2 = \frac{n \cdot D ^ 2}{2 \cdot a \cdto \sigma^2} $
  
  double phisq = (n * pow(D, 2)) / (2 * a * pow(sigma, 2));
  
  return phisq;
}