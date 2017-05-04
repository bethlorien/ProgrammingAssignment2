## These functions will cache the inverse of a matrix 
## so it only has to be computed once

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(inputmatrix = matrix()) {
    invertedmatrix <- NULL
    
    # function to set a new input matrix and set invertedmatrix to NULL
    set <- function(y) {
        inputmatrix <<- y
        invertedmatrix <<- NULL
    }
    
    # function to get the input matrix
    get <- function() inputmatrix
    
    # function to set the inverted matrix to solved matrix
    setinverse <- function(solvedmatrix) invertedmatrix <<- solvedmatrix
    
    # function to return value of invertedmatrix
    getinverse <- function() invertedmatrix
    
    # returns list of these four functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse or retrieves it from the cache.
## The input argument should be the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invertedmatrix <- x$getinverse()
    if(!is.null(invertedmatrix)) {
        message("Getting cached matrix inverse.")
        return(invertedmatrix)
    }
    data <- x$get()
    invertedmatrix <- solve(data, ...)
    x$setinverse(invertedmatrix)
    invertedmatrix
}
