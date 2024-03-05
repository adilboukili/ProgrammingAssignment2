## Put comments here that give an overall description of what your
## functions do

## Put comments here that give an overall description of what your
## functions do

# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    getInverse <- function() inverse
    setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
    list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}

# Function to compute the inverse of the special "matrix" object
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}


