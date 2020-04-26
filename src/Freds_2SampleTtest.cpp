// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

double Freds_2SampleTtest(const arma::colvec & x1,
                          const arma::colvec & x2) {
    int n1, n2;
    double xbar1, xbar2, v1, v2, t;
    n1 = x1.size();
    n2 = x2.size();
    v1 = pow(arma::stddev(x1), 2);
    v2 = pow(arma::stddev(x2), 2);
    xbar1 = mean(x1);
    xbar2 = mean(x2);
    
    t = (xbar1 - xbar2) / pow((v1 / n1) + (v2 / n2), 0.5);
    
    return t;
}
