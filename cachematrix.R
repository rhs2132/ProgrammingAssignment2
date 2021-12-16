## Put comments here that give an overall description of what your
## functions do

## Creates a CacheMatrix object which is actually a list of 4 functions for accessing the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setInverse <- function(invers) inv <<- invers
    getInverse <- function() inv
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Returns the inverse of a CacheMatrix either from the cache or computing de novo
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("retrieving cached inverse")
        return(inv)
    } else {
        message("calculating inverse de novo")
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        return(inv)
    }
}
