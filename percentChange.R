#Creates a vector that gives the percent change from  each consecutive element
percentChange <- function(x){
  #Create a vector just like x but without the first element
  u <- x[-1]
  #Create a vector just like x but without the last element
  v <- x[-length(x)]
  #Subtract v from u and divide by v
  perChange <- (u - v)/v*100
  return(perChange)
}

#Example:
#x <- c(1,2,2.5,3.5, 10, 9)
#percentChange(x)
