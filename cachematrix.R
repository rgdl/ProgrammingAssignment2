## Put comments here that give an overall description of what your
## functions do

# - makeCacheMatrix creates a 'special' matrix that can cache the value of its inverse, 
# so that the inverse doesn't have to be unnecessarily recalculated.
# - cacheSolve calculates the inverse of the 'special' matrix, unless an inverse is 
# already cached, in which case it returns that.


## Write a short comment describing this function
# This function creates a list containing several functions that act on the 'special' matrix.
# These functions in the list can get or set the matrix and get or set the inverse
# The 'special' matrix is stored within the scope of the function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function (newInv) inv <<- newInv
    getInv <- function () inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function
# This function returns the cached inverse if there is one cached. Otherwise, it calculates the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message('getting cached data')
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}