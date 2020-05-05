Sigma_tau_squared <- function(df, a) {
  # $ \sum_{i=1}^{a} \tau_{i}^{2} $
  
  mean_list <- tapply(df$observations, df$treatment, mean)
  mu_bar <-  mean(mean_list) / a
  sig_tau_sq <- sum((mean_list - mu_bar)^2)
  
  return(sig_tau_sq)
}