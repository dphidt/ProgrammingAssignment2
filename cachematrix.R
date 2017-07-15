## Matrix inverse caching.
##
## Functions to create a specialized matrix-like object that can
## cache its inverse, and access the cached version. Caching will
## improve performance when the matrix inverse is repeatedly
## required in a calculation, as it is expensive to compute.
##
## This is a solution to Programming Assignment 2: Lexical Scoping,
## for the Coursera course "R Programming."
##
## dphidt, GitHub @dphidt, 2017/07/15


## Create a matrix-like object that can cache its inverse.
##
## The return value is a list \code{x} with getter/setter functions
## for working with the matrix:
##
##     * x$get()     Returns the matrix itself
##     * x$getinv()  Returns the inverse if available, otherwise NULL
##     * x$set(y)    Set the matrix to \code{y}
##     * x$setinv(z) Set the matrix inverse to \code{z}
##
## @param x The original matrix
## @return A list with getter/setter functions (see details above)
makeCacheMatrix <- function(x = matrix()) {
    # The inverse is initialized to NULL, and computed on the first
    # call to cacheSolve.
    inv <- NULL

    # A function to set the matrix (and clear the inverse)
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # A function to return the matrix
    get <- function() x

    # A function to set the matrix inverse
    setinv <- function(inv_) inv <<- inv_

    # A function to return the matrix inverse
    getinv <- function() inv

    # Return a list of the getter/setter functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Get the inverse matrix for \code{x}.
##
## If the inverse is cached in \code{x}, return the value from
## cache. Otherwise, compute the inverse, store it inside
## \code{x} for later reference, and return it.
##
## Additional arguments are passed to the \code{solve} function
## which computes the inverse.
##
## @param x The object to compute the inverse of x (an object
##          created using makeCacheMatrix)
## @return The inverse matrix
cacheSolve <- function(x, ...) {
    # Get the cached inverse from x
    inv <- x$getinv()

    # If the cache is non-NULL, return it
    if (!is.null(inv)) {
        message('getting cached data')
        return(inv)
    }

    # Cache is NULL, so we must compute the inverse and store it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
