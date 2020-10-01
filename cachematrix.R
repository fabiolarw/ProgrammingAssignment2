## This script corresponds to Programming Assignment 2: Lexical Scoping
## from Coursera's course "R programming"
##
## Note: Only works when matrix supplied is invertible.
## It does not make a prior verification.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## This part will be inspired by makevector function
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invmat <<- inverse
  getInverse <- function() invmat
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## This part will be inspired by cachemean function
  invmat <- x$getInverse()
  if (!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  ## if X is a square invertible matrix, then solve(X) returns its inverse.
  invmat <- solve(data, ...)
  x$setInverse(invmat)
  invmat
}