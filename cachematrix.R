## This file provides functions to cache the inverse of a matrix.
## Matrix inversion is usually a costly computation and there may be a benefit
## caching the inverse rather than computing it repeatedly.
##

## This function returns a list with the functions to set, get a CacheMatrix and 
## the functions used to set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y){
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve){
        s <<- solve
    }
    getSolve <- function() s
    list ( set = set , get = get , 
           setSolve = setSolve,
           getSolve = getSolve )
}


## CacheSolve is the function that actually does the cache, instead of calling solve call cacheSolve 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve()
    if( !is.null(s)) {
        message("Getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data)
    x$setSolve(s)
    s
}
