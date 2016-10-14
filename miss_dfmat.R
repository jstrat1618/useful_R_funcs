#A function that returns a list containg the total missing elements in a data frame and
#the total by rowsand columns

miss_dfmat <- function(dat){
  if( !(is.matrix(dat) | is.data.frame(dat)) ) stop('dat must be a data frame or matrix')
  tempDat <- is.na(dat)
  totMiss <-  sum(tempDat)
  colMiss <- apply(tempDat,2,sum)
  rowMiss <- apply(tempDat,1,sum)
  myList <- list(totMiss = totMiss, colMiss = colMiss, rowMiss=rowMiss )
  return(myList)
                     }

#Examples:
#dat <- data.frame(v1=c(1,2,NA,4), v2=c('A', 'B', NA, 'D'))
#mat <- as.matrix(dat)
#miss_dfmat(dat)
#miss_dfmat(mat)

