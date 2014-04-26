## Caching the inverse of a matrix
## The two functions below are two are used to create a special object that
## stores a matrix and cache's its mean.

## makeCacheMatrix creates a special "matrix", which is really a list
## containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
##

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(matInv) inv <<- matInv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The function cacheSolve computes the inverse of the special "matrix" created
## with the function makeCacheMatrix. It first checks to see if the inverse has
## already been computed If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it computes the inverse of the matrix and sets the
## value of the matrix in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
