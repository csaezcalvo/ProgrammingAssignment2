## The first function makeCacheMatrix creates an object representing a matrix
## that can cache its inverse and the second function computes its inverse

## Creates an object with four functions
## set() sets the value of the matrix
## get() returns the value of the matrix
## setinv() allows to set the value of the inverse of the matrix
## getinv() returns the set value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Takes as input a "CacheMatrix" object and returns the value of the inverse
## from the cache if it has been computed, or computes it and sets it to the
## cache if not

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cache data")
    return(inv)
  }
  data <- x$get()
  xinv <- solve(data,...)
  x$setinv(xinv)
  xinv
}
