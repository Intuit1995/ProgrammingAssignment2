## R Programming:  Program Assignment 2
## Caching the Inverse of a Matrix

## makeCacheMatrix:  This function creates a special "matrix" object 
##                   that can cache its inverse.
##                   output of this function can be referenced in cachesolve.

makeCacheMatrix <- function(x = matrix()) {
  inv.matrix <- NULL
  set <- function(y) {
          x <<- y
          inv.matrix <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv.matrix <<- solve   
    getinv <- function() inv.matrix
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}

## cacheSolve:  This function computes the inverse of the special "matrix" returned by
##              makeCacheMatrix.  If the inverse has already been calculated for a given
##              matrix, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ##Acquire Inverted Matrix from makeCacheMatrix
    inv.matrix <- x$getinv()
    if(!is.null(inv.matrix)) {
        message("getting cached data")
        return(inv.matrix)
    }
    
    ##If makeCacheMatrix does not have inverted matrix, it is calculated below.
    data <- x$get()
    inv.matrix <- solve(data,...)
    
    ##Capture inverted matrix, and establish it in makeCacheMatrix for future use.
    x$setinv(inv.matrix)
    return(inv.matrix)
}
