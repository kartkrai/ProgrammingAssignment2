## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x) {
  
  cofact<- x     ## declaring cofactor matrix 
  d<- c(1:nrow(x))
  cacheSolve(x, cofact, d)
}

## Write a short comment describing this function

cacheSolve <- function(x,cofact, d) {
  
  for (a in d) {
    
    for (b in d) {
      if((b+a)%%2==0)
        cofact[a,b]<- (det(x[-a,-b]))   ## calculating cofactor matrix 
      if((b+a)%%2==1)
        cofact[a,b]<- -(det(x[-a,-b]))   ## calculating cofactor matrix 
    }
    
  }
  
  cofact/(det(x))     ## Returns a matrix that is the inverse of 'x'
}
