Freds_TukeyHSD <- function(observations, treatments) {
  df <- data.frame(
    obs = observations,
     tr = treatments 
  )
  
  ##### Mean Squares Within ####
  MSW <- MeanSquare_Within(df)
  
  hsd_list <- list()
  l_index <- 1
  for (treatment_i in levels(df$tr)) {
    n <- length(df$obs[df$tr == treatment_i])
    mi <- mean(df$obs[df$tr == treatment_i])
    for (treatment_j in levels(df$tr)) {
      if (treatment_i != treatment_j) {
        mj <- mean(df$obs[df$tr == treatment_j])
        hsd_list[l_index] <- TukeysQ(mi, mj, MSW, n)
      }
    }
  }
  return(hsd_list)
}