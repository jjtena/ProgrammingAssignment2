## These functions return the inverse of a matrix, but if it
## was already calculated they get the solution from the cache


## This function creates an object from a matrix that is a list
## of functions to set the matrix, get the matrix, set its inverse
## and get this inverse matrix.

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
             setinv = setinv,
             getinv = getinv)
}


## This function calculate the inverse of an object created from a matrix
## with the makeCacheMatrix function and steres it in cache. If it was 
## already calculated it gets the solution from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
