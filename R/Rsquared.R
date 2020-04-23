Rsquared <- function(x, y){
  # SSxy/sqrt(SSxx SSyy)
 
  SSxy <- SumSquares_xy(x, y)
  SSxx <- SumSquares(x)
  SSyy <- SumSquares(y)
  
  r <- SSxy / sqrt(SSxx * SSyy)
  
  return(r^2)
   
}