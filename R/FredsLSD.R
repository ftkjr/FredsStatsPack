FredsLSD <- function(observations, treatments) {

  df <- data.frame(
    obs = observations, 
    tr = treatments
  )
  
  ##### Mean Squares Within ####
  MSW <- MeanSquare_Within(df)
  
  ##### Combinations ####
  for (i in levels(treatments)){
    for (j in levels(treatments)){
      if (i != j) {
        n1 <- length(df$obs[tr == i])
        n2 <- length(df$obs[tr == j])
        mu1 <- mean(df$obs[tr == i])
        mu2 <- mean(df$obs[tr == j])
        
        ##### t statistic ####
        t <- (mu1 - mu2) / sqrt(MSW * (1/n1 + 1/n2))
      }
    }
  }
  

  
  
  
  
  
} 