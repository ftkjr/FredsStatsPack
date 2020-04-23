// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
#include "RcppArmadillo.h"

// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::export]]
double SigmaTau(arma::vec mu) {
    double mu_bar, sigma_tau;
    int n = mu.size();
    mu_bar = mean(mu);
    for (int i = 0; i < n; i++) {
        sigma_tau += mu[i] - mu_bar;
    }
     
	                     
    return sigma_tau;
}