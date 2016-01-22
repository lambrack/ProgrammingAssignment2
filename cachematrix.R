## These functions solve for the inverse of a matrix and
## cache the results to minimize the need for repeated
## computations.


## makeCacheMatrix returns a list that contains functions
## which set and get the values of the matrix and its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
}


## cacheSolve first identifies if the inverse has been
## previously solved (and cached), and returns that inverse.
## Otherwise, it calculates the inverse and stores the value
## into the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
