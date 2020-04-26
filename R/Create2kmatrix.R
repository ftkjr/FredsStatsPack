Create2kmatrix <- function(k) {
  if (k > 4) {
    stop("This function handles only 2^4 currently.\n
         Please contact Fred if it needs to be extended.")
  }
  ##### How many runs? ####
  # 2^k factorial model has 2^k runs
  runs <- 2^k
  
  ###### Initialize -,+ matrix ####
  sigmat <- matrix(nrow = runs,
                   ncol = runs - 1)
  
  ##### For the first k columns ####
  for (column in c(1:k)){
    sigmat[, column] <- rep(c(rep(-1, 2^(column-1)), rep(1, 2^(column-1))), runs/(2^column))
  }
  
  ##### For next k choose 2 columns ####
  l <- k+1
  for (i in c(1:k)){
    for (j in c(2:k)) {
      if (j > i) {
        #cat(l, i, j, "\n")
        sigmat[, l] <- sigmat[, i] * sigmat[, j]
        l <- l+1
      }
      
    }
  }
  
  ##### For the k choose 3 columns after that ####
  if (k > 2) {
    for (i in c(1:k)) {
      for (j in c(2:k)) {
        for (k in c(3:k)) {
          if (k > j & j > i) {
            sigmat[, l] <- sigmat[, i] * sigmat[, j] * sigmat[, k]
            l <- l+1
          }
        }
      }
    }
  }
  
  ##### The final column if k == 4 ####
  if (k > 3) {
    sigmat[, 15] <- sigmat[, 1] * sigmat[, 2] * sigmat[, 3] * sigmat[, 4]
  }
  
  return(sigmat)
}