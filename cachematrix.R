## The functions take an invertible matrix, invert it, and cache
## the result in an object if it is not already stored there.


## Creates an object to store the original matrix and the inverted matrix.
## Defines getter and setter methods.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) {i <<- inv}
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Checks if inverted matrix has already been calculated. 
## If no, it inverts the matrix and stores it in the object created through makeCacheMatrix.
## If yes, it gets the stored result from the object created through makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
