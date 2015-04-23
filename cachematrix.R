## makeCacheMatrix creates a matrix object that can cache its inverse.
## cacheSolve returns the inverse of the matrix that is defined this way. If the
## inverse has already been calculated, then the function retrieves the cached
## inverse matrix, otherwise it calculates the inverse, caches it, and returns
## its value. Caching inverse of a matrix reduces the computational cost of
## calculating it repetitively.


## Returns a matrix object that can cache its inverse. Get and set functions are
## defined to allow access to the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    Set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    Get <- function() x
    SetInv <- function(inverse) inv <<- inverse
    GetInv <- function() inv
    return(list(Set = Set, Get = Get,
                SetInv = SetInv,
                GetInv = GetInv))
}


## Returns the inverse of a matrix object defined with makeCacheMatrix. (from
## the cache if it has already been calculated.)

cacheSolve <- function(x, ...) {
    inv <- x$GetInv()
    if (!is.null(inv)) {
        message("Getting cached data")
    } else {
        mat <- x$Get()
        inv <- solve(mat, ...)
        x$SetInv(inv)
    }
    return(inv)
}
