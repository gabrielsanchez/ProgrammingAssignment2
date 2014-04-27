##import library for inverse operation
##http://stat.ethz.ch/R-manual/R-patched/library/MASS/html/ginv.html
library(MASS)

##set/get values of matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##calculate the inverse of the special matrix
cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- ginv(data, ...)
  x$setinverse(i)
  i
}
