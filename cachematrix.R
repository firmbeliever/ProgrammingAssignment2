## These functions take advantage of the scoping rules to allow 
## the caching of a calculated variable so that it doesn't have
## to be recalculated every time it is needed.  This is particularly
## helpful for CPU-intensive calculations, such as a matrix
## inverse.

## This functions creates a special matrix object that includes
## getter and setter functions that can access variables scoped
## within the function, which are used to cache the caculated
## matrix inverse for a given input variable

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function is used to get the inverse of a matrix
## using the caching function above.  It will calcualte the 
## inverse if the value has not already been cached or simply
## retrieve the value from the cache (without recalculating)
## if the cached value already exists

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
