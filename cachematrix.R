## Pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # to store inverse value
    inverse <- NULL
    # set the original matrix and reset inverse by setting it to NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    # get the original matrix "x"
    get <- function()
        x
    # set inverse value
    set_inverse <- function(inv)
        inverse <<- inv
    # get inverse value
    get_inverse <- function()
        inverse

    # Returns a list of the 4 functions, this list is the special "matrix"
    list(
        set = set,
        get = get,
        set_inverse = set_inverse,
        get_inverse = get_inverse
    )
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated
## (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    # check if inverse is NOT cached
    if (!is.null(inverse)) {
        message("getting cached data")
        # get/return cached inverse
        return(inverse)
    }
    data <- x$get()
    # we use solve(x)
    # we assume that the matrix is
    # always a square invertible matrix
    # for this assignment
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
    ## Return a matrix that is the inverse of 'x'
    inverse
}
