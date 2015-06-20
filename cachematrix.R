## Caches the inverse of a matrix to avoid repeating time consuming operations.

## creates a list with the original matrix, a space for it's inverse, and functions to read and store both. 

makeCacheMatrix <- function(x = matrix()) {
    m.inverse <- NULL
    
    set <- function(y) {
        x <<- y
        m.inverse <- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(new.inverse) {
        m.inverse <<- new.inverse
    }
    getinverse <- function() {
        m.inverse
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Determines the inverse of the matrix stored by makeCacheMatrix and stores it in the space created by the function.

cacheSolve <- function(x, ...) {
    m.inverse <- x$getinverse()
    if(!is.null(m.inverse)) {
        message("Getting cahced data")
        return(m.inverse)
    }
    working.matrix <- x$get()
    m.inverse <- solve(working.matrix, ...)
    x$setinverse(m.inverse)
    m.inverse
}
