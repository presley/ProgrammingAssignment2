## below 2 functions that cache the inverse of a matrix,
## 'makeCacheMatrix' and 'cacheSolve'

## this function creates a special matrix object to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) n <<- inverse
  getinverse <- function() n
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## this function computes the inverse of the special matrix returned
## by the function makeCacheMatrix, if inverse has been calculated
## already, it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  n <- x$getinverse()
  if(!is.null(n)) {
    message("getting cached data.")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinverse(n)
  n
}
