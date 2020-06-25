## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invmtrx <- NULL
    set <- function(y) {
        x <<- y
        invmtrx <<- NULL
    }
    get <- function() x
    setinvmtrx <- function(i) invmtrx <<- i
    getinvmtrx <- function() invmtrx
    list(set = set, get = get,
         setinvmtrx = setinvmtrx,
         getinvmtrx = getinvmtrx)      
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invmtrx = x$getinvmtrx()
    if(!is.null(invmtrx)) {
      message("getting cached data")
      return(invmtrx)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinvmtrx(i)
    i
}
