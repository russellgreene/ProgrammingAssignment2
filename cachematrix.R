## The makeCacheMatrix creates an object which contains 
## getter and setter functions for the inverse of a matrix

## makeCacheMatrix creates an object which contains getter
## and setter functions for both matrix itself as well as
## the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get=get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns a matrix that it is the inverse of 'x'
## As a performance improvement it will use the cachedValue
## when available instead of re-computing the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    rawMatrix <- x$get()
    inverse <- solve(rawMatrix, ...)
    x$setinverse(inverse)
    inverse
}
