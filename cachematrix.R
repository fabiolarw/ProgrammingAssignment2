## This script corresponds to Programming Assignment 2: Lexical Scoping
## from Coursera's course "R programming"
##
## Note: Only works when matrix supplied is invertible.
## It does not make a prior verification.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## This part will be inspired by makevector function
  ## initialize variable
  invmat <- NULL
  
  ##set value of the matrix
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  
  ##get value of the matrix
  get <- function() x
  
  ##set value of the inverse matrix
  setInverse <- function(inverse) invmat <<- inverse
  
  ##get value of the inverse matrix
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
  
  ##Get the matrix
  data <- x$get()
  
  ## Calculate the inverse matrix.
  ## If X is a square invertible matrix, then solve(X) returns its inverse.
  invmat <- solve(data, ...)
  
  ##Set the inverse matrix
  x$setInverse(invmat)
  
  invmat
}