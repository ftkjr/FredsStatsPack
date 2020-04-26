MeanSquare_Within <- function(df) {
  ##### Mean Squares Within ####
  SSW <- 0
  for (level in levels(df$tr)) {
    SSW <- SSW + (df$obs[df$tr == level] - mean(df$obs[df$tr == level]))^2
  }
  SSW <- sum(SSW)
  MSW <- SSW / degreesfreedom_Den
  
  return(MSW)
}