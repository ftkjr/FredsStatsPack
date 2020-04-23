#include "RcppArmadillo.h"
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]


double ttest_means(double y1, double y2,
                   double n1, double n2, 
                   double mse){
  double tstat = (y1 - y2)/ pow(mse * (1/n1 + 1/n2), 0.5);
  
  return tstat;
}