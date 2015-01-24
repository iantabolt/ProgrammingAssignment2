## cachematrix.R

## Implementation of a matrix wrapper that
## caches its inverse.

## Pseudo constructor returns a wrapped matrix
## object that can cache its inverse.
## Implementation note: I did not include all the getters
## and setters like in the vector/mean example
## because it is not part of the assignment,
## and because data abstraction is good.
makeCacheMatrix <- function(x = matrix()) {
    # private value cachedinv will
    # save the result of the inverse
    # after the first time it is calculated
    cachedinv <- NULL

    # private value params will save the parameters
    # used to calculate the cached inverse,
    # since if the paramters change, the cached
    # result needs to be recalculated
    params <- NULL

    # returns the unwrapped matrix data
    get <- function() x

    # returns the inverse of the matrix.
    # saves the result so we only need to calculate it once
    cacheSolve <- function(...) {
        # recalculate if this is the first time calling,
        # or if the parameters have changed
        if (is.null(cachedinv) || !identical(list(...), params)) {
            message("Calculating inverse")
            cachedinv <<- solve(x, ...)
            params <<- list(...)
        }
        cachedinv
    }
    
    list(get = get,
         cacheSolve = cacheSolve)
}

## Computes the inverse of the special "matrix" returned by
## makeCacheMatrix.
cacheSolve <- function(x, ...) {
    x$cacheSolve(...)
}
