PhiSquared <- function(observations, treatment, sigma = NULL){
  # $\Phi^2 = \frac{n \sum_{i=1}^a \tau_i^2}{a \sigma^2}$
  df <- data.frame(
    observations = as.numeric(observations),
    treatment = as.factor(treatment)
  )
  a <- nlevels(df$treatment)
  if (!is.null(sigma))  sigma <- sd(df$observations)
  sigma_tau_squared <- Sigma_tau_squared(df, a)
  n <- length(df$observations)
  
  phi_squared <- (n * sigma_tau_squared) / (a * sigma^2)
  
  return(phi_squared)
}