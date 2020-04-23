FredsLSD <- function(group1, group2) {
  ##### Sample length ####
  n1 <- length(group1)
  n2 <- length(group2)
  
  ##### Group Means ####
  mu1 <- mean(group1)
  mu2 <- mean(group2)
  
  ##### Mean Squares Within ####
  SSW <- 0
  for (level in levels(df$treat)) {
    SSW <- SSW + (df$obs[df$treat == level] - mean(df$obs[df$treat == level]))^2
  }
  SSW <- sum(SSW)
  MSW <- SSW / degreesfreedom_Den
  
  ##### t statistic ####
  t <- (mu1 - mu2) / sqrt(MSW * (1/n1 + 1/n2))
  
  
} 