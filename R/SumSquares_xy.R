SumSquares_xy <- function(x, y) {
  # Warning: Not technically a sum of squares
  xySum <- as.data.frame(x,y) %>%
    mutate(xdev = x - mean(x),
           ydev = y - mean(y),
           combdev = xdev * ydev) %>%
    summarise(sum(combdev))
  
  return(xySum)
}