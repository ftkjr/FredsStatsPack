// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"


// [[Rcpp::depends(RcppArmadillo)]]



// [[Rcpp::export]]




double EffectSize_t(double mu1,
                    double mu2,
                    double sigma) {
    // Definition: Effect size is a quantitative measure 
    //   of the magnitude of the experimenter effect.
    
    // The larger the effect size, the stronger the 
    //   relationship between two variables. 
    
    double d = (mu1 - mu2) / sigma;
    return d;
}

