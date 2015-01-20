## The functions in this script will create a matrix and cache its
## inverse.

## When the matrix is first created, the inverse will not
## be calculated until cacheSolve() is called.
## When cacheSolve() is called, the inverse is computed and returned.
## If cacheSolve() is called on a matrix who's inverse was already computed,
## the solution will be taken from the cache to reduce computational effort.


# Take a matrix and create a matrix that is able to store its own inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Create a matrix that can store its own inverse, assuming the matrix
    ## does not change between calls
    
    # Initialize the inverse to NULL until it is set by the
    # setinv() function
    inv <<- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


# Get the inverse-matrix from a matrix created by makeCacheMatrix().
# If this function has already been called on a given matrix, then the
# solution is taken from the cache instead of computing it again.

cacheSolve <- function(x, ...) {
    ## Return the inverse-matrix of 'x'
    
    # Retrieve the cached inverse if it has been computed before.
    inv <- x$getinv()
    if (!is.null(inv)) {
        message('getting cached inverse')
        return(inv)
    }
    
    # Compute the inverse if it hasn't been computed before, and cache it
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}










