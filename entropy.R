#entropy for categorical data
entropy=function(x){
  if( !is.character(x) & !is.factor(x) ) stop('x must be of class character or factor')
  v <- table(x) #missing values will be omitted in the table
  #Compute the total non-missing entries
  n <- length(x[!is.na(x)])
  ent <- sum(v/n*-log(v/n))
  return(ent)
}

#Example
#x <- c('M', 'M', 'F', 'F')
#entropy(x)


