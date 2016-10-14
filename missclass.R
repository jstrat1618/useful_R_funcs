#This function returns the total number of misclassifcations
missclass=function(pred, obs){

  #Create a warnings in the case that pred or obs is numeric
  if(is.numeric(pred)) warning('pred has class numeric')
  if(is.numeric(obs)) warning('obs has class numeric')
  #Stop if length pred is not equal to length obs  
  if(length(pred) != length(obs)){ 
    stop('Both predicted and observed data should have the same length')}
    
  #Create a matrix of predicted vs observed
  mat<-as.matrix(table(pred,obs))
  diag(mat)<-0
  totMiss<-sum(mat)
  return(totMiss)
}

#Example: missclass(c('a','a','b', 'c'), c('a', 'a', 'c', 'b') )
