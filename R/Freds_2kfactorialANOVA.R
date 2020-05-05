Freds_2kfactorialANOVA <- function(effect_matrix, replications_matrix){
  ##### Initialize Variables ####
  # 2^k factorial
  k <- log2(nrow(effect_matrix))
  n <- ncol(replications_matrix)
  
  # Total across rows
  rep_totals <- rowSums(replications_matrix)
  effects_and_replications <- cbind(effect_matrix, replications_matrix)
  
  # Empty ANOVA Matrix
  twok_anova_mat <- matrix(
    ncol = 5,
    nrow = 2 ^ k + 1)
  
  # Row and Column Names
  if (k == 2) {
    dimnames(twok_anova_mat) <- list(
      # Row Names
      c("A", "B", "AB", "Error", "Total"),
      # Column Names
      c("df", "SS", "MS", "F", "p"))
  } else if (k == 3) {
    dimnames(twok_anova_mat) <- list(
      # Row Names
      c("A", "B", "C", "AB", "AC", "BC", "ABC", "Error", "Total"),
      # Column Names
      c("df", "SS", "MS", "F", "p"))
  } else if (k == 4) {
    dimnames(twok_anova_mat) <- list(
      # Row Names
      c("A", "B", "C", "D", "AB", "AC", "AD", "BC", "BD", "CD", "ABC", "ABD", "BCD", "Error", "Total"),
      # Column Names
      c("df", "SS", "MS", "F", "p"))
  }
  
  ##### Contrasts ####
  for (row in c(1:2^k-1)) {
    # Effect df is 1
    twok_anova_mat[row, 1] <- 1
    
    # SS
    twok_anova_mat[row, 2] <- sum(rep_totals * effects_and_replications[, row]) ^ 2 / (2^k * n)
    
    # MS
    twok_anova_mat[row, 3] <- twok_anova_mat[row, 2] / twok_anova_mat[row, 1]
  }
  
  ##### Sum Squares Error and Total ####
  # df Total 
  twok_anova_mat[2^k+1, 1] <- length(replications_matrix) - 1
  
  # df Error
  twok_anova_mat[2^k, 1] <- twok_anova_mat[2^k+1, 1] - sum(twok_anova_mat[c(1:2^k-1), 1])
  
  # SS Total $\sum (all_rep - mean_allrep)^2$
  twok_anova_mat[2^k+1, 2] <- sum(
    (replications_matrix[,c(1:ncol(replications_matrix))] - mean(replications_matrix[,c(1:ncol(replications_matrix))]))^2
    )
  
  # SS Error SST - \sum SSContrasts
  twok_anova_mat[2^k, 2] <- twok_anova_mat[2^k+1, 2] - sum(twok_anova_mat[c(1:2^k-1), 2])
  
  # in case of saturated model 
  if (twok_anova_mat[2^k, 2] == 0) {
    warning("Saturated Model\nUsing interaction effect as Error Term.")
    denominator <- twok_anova_mat[2^k - 1, 3]
  } else {
    # MS Error
    twok_anova_mat[2^k, 3] <- twok_anova_mat[2^k, 2] / (twok_anova_mat[2^k, 1])
    
    denominator <- twok_anova_mat[2^k, 3]
  }
  
  
  
  ##### F statistic ####
  twok_anova_mat[c(1:2^k-1), 4] <- round(twok_anova_mat[c(1:2^k-1), 3] / denominator, 3)
  
  ##### p value ####
  
  if (twok_anova_mat[2^k, 2] == 0) {
    df <- 1
    twok_anova_mat[c(1:2^k-1), 5] <- round(digits=5,
        1 - pf(twok_anova_mat[c(1:2^k-1), 4], df, df))
  } else {
      twok_anova_mat[c(1:2^k-1), 5] <- round(digits=5,
        1 - pf(twok_anova_mat[c(1:2^k-1), 4], twok_anova_mat[c(1:2^k-1), 1], twok_anova_mat[2^k+1, 1]))
  }
  
  return(twok_anova_mat)
}