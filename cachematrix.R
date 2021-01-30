## Put comments here that give an overall description of what your
## functions do

## Description makeCacheMatrix():
## Function to which initializes a matrix as an argument.
## It defines the variable inv as an empty matrix.
## Function set() is defined to set a new matrix to the cache and reset inv as empty
## Function get() is defined to get the matrix in the cache
## Function setinv() is defined to set a new inverse matrix to the cache
## Function getinv() is defined to get the inverse matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  set <- function(y) {
    x <<- y
    inv <<- matrix()
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Description cacheSolve():
## Function to which returns a matrix that is the inverse of 'x'
## It takes 'x' as an argument.
## It calls getinv() for x which defined in makeCacheMatrix() to get the inverse
## matrix of 'x' if it is already in cache.
## If statement to check, if an inverse exists in cache
## If the inverse in cache is empty. The inverse of 'x' is calculated and set to cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!all(is.na(inv))) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


