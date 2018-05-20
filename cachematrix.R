## Pair of functions below are used to create a special object that stores a
## matrix and caches its inverse

## The first function makeCacheMatrix creates a list of functions that
## act as a special "matrix" 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinvert <- function(invert) i <<- invert
        getinvert <- function() i
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}

## The function cacheSolve calculates the inverse of the special "matrix" and
## sets the value in the cache.  If the inverse has already been calculated, 
## it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
        i <- x$getinvert()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinvert(i)
        i
        ## Return a matrix that is the inverse of 'x'
        }