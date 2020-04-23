#' @title FredsANOVA
#'
#' @param observations a numeric vector
#' @param treatments a vector to be converted to factors if not already
#'
#' @return A list of ANOVA information
#' 
#' @name Frederick T Kaesmann Jr. 
#' @note Developed for use in Dr. Yulei Pang's MAT 325 Design of Experiments
#' @note I got bored during a quarantine, what can I say?
#' 
#' @examples FredsANOVA(df$value, df$variable)
FredsANOVA <- function(observations, treatments){
  
  ##### Correct Data Types ####
  observations <- as.numeric(observations)
  treatments <- as.factor(treatments)
  
  ##### Make a New Data Frame ####
  df <- data.frame(
    obs = observations,
    treat = treatments
  )
  
  ##### N, k ####
  N <- length(df$obs)
  k <- nlevels(df$treat)
  
  ##### Degrees of Freedom ####
  degreesfreedom_Num <- k - 1
  degreesfreedom_Den <- N - k
  
  ##### Sum Squares Within ####
  ssw_list <- tapply(df$obs, df$treat, Sum_Squares)
  SSW <- sum(ssw_list)
  
  ##### Mean Squares Within ####
  MSW <- SSW / degreesfreedom_Den
  
  ##### Sum Squares Between ####
  SSB <- 0
  for (level in levels(treatments)) {
    n <- length(df$obs[df$treat == level])
    SSB <- SSB + n * (mean(df$obs[df$treat == level] - mean(df$obs)))^2
  }
  
  ##### Mean Square Between ####
  MSB <- SSB / degreesfreedom_Num
  
  ##### F Stat ####
  F_statistic <- MSB / MSW
  
  ##### p - value ####
  p <- pf(q = F_statistic,
        df1 = degreesfreedom_Num, 
        df2 = degreesfreedom_Den,
        lower.tail = FALSE)
  
  ##### List of Important Info ####
  info <- list(
    "Degrees of Freedom: Numerator" = degreesfreedom_Num,
    "Degrees of Freedom: Denominator" = degreesfreedom_Den,
    "Sum Square Between" = SSB,
    "Mean Square Between" = MSB,
    "Sum Square Within" = SSW,
    "Mean Square Within" = MSW,
    "F_statistic" = F_statistic,
    "p_value" = p
  )
  
  ##### Return ANOVA Info ####
  return(info)
}