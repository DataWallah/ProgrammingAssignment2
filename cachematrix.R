# Inverts a matrix; assumes matrix is always invertible
# assume we always get passed a square matrix
# Because of assumptions no parameter or error checking

# Must call 'makeCacheMatrix' to set up the matrix
# before calling 'cacheSolve' to compute the inverse



makeCacheMatrix <- function(x = matrix()) {
        # initialize and define the functions
        inv <- NULL                       
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        # set up the list
        list(set = set, get = get,         
             setinv = setinv,
             getinv = getinv)
}
cacheSolve <- function(x, ...) {
        # get the current value of inv
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        # new matrix so compute the inverse and print
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
