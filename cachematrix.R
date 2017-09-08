## This is a set of two functions that let inverse a matrix, store it
## in a cache and reuse the cache without recalculating. 

## makeCacheMatrix is an intermediate function that returns a list
## of four functions that are used later on in creating and retrieving
## a cache for inversed matrix:
## - set() which sets a new matrix to be inversed
## - get() which gets the current matrix to be inversed
## - setinv() which sets the calculated inversed matrix as cached result
## - getinv() which retrieves the current cache
## It takes one argument - a matrix to be inversed

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solved) inv <<- solved
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function is the final function which:
## - calculates the inverted matrix and puts it into cache if the cache 
##   is not yet present
## - returns the cached inverted matrix if the cache is already present 
## It takes as argument an object of type makeCacheMatrix and optionally 
## other parametres of the solve() function 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}