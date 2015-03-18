## This is a programming assignment that demonstrates the use of lexically
## scoped variables for caching. It defines functions that let you create a 
## matrix object that caches its inverse.

## Creates a special "matrix" object that can cache its inverse, for performance.

## This matrix is really a list that contain functions that close over the
## underlying matrix and cache data

makeCacheMatrix <- function(x = matrix()) {
  # Cache variable that will hold the matrix inverse
  cachedInverse <- NULL
  # Getter/setter functions that "close over" cachedInverse
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) cachedInverse <<- inv
  getInverse <- function() cachedInverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Solves for the inverse of a matrix, using and maintaining cached
## results
## Input:
##   x  - a special inverse-caching matrix created by makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if ( ! is.null(inv) ) {
    message("cache hit")
    inv
  } else {
    message("cache MISS")
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
  }
}
