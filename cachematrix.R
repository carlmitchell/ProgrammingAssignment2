## This pair of functions makes it possible to cache
## the inverse of a matrix for later use. Since matrix
## inversion can be computationally expensive, it is
## desirable to do this so that the inverse function
## does not need to be called multiple times.

## This first function creates a new object from a
## matrix with the ability to have a cached inverse.
## This object is a list of 4 functions for interacting
## with the matrix and its inverse.

makeCacheMatrix <- function(m = matrix()) {
    # First we initialize the inverse to NULL
    inv <- NULL
    
    # Now we define a "set" function for our object
    # This sets our matrix to the value in y and
    # overwrites a stored inverse with NULL.
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
    
    # Next a "get" function to return the matrix
    get <- function() m
    
    # Now a function to set the inverse
    setinv <- function(inverse) inv <<- inverse
    
    # And a "get" function to return the inverse
    getinv <- function() inv
    
    # Finally, return these 4 functions as a list
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## This function returns the cached version of the
## inverse if the matrix has not changed, or calculates
## the inverse and caches it if it has.

cacheSolve <- function(m, ...) {
    # Get the cached inverse if it exists
    inv <- m$getinv()

    # Return the cached inverse if it exists.
    if(!is.null(inv)){
        message("Retrieving cached inverse.")
        return(inv)
    }
    
    # If we're still here, we have no cached inverse
    # so we should calculate it and return it
    matrix <- m$get()
    inv <- solve(matrix)
    m$setinv(inv)
    inv
    
}
