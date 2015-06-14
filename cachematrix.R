## This pair of functions are constructed to make use of caching
## to avoid the unnecessary re-calculation of inverse matrix,
## which can be slow resource consuming.

## The first function creates a special "matrix", which is 
## a list of 4 functions to set and get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The second function calculates an inverse of a special
## "matrix", created by the first function. If the matrix is
## unchanged, the function returns the cached value. Otherwise
## inverse is recalculated and cached again.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if ( !is.null(inv) ) {
        message( "getting cached inverse" )
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
