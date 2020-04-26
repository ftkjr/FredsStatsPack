#include "RcppArmadillo.h"

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]

double Sum_Squares(const arma::colvec & x) {
  
  int n = x.size();
  double avg, sum_sq;
  
  avg = mean(x);
  
  for(int i = 0; i < n; i++){
    
    sum_sq += pow(x[i] - avg, 2);
    
  }
  
  return sum_sq;
}