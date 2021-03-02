## Programming Assignment #2 for R Programming Week 3
## Class provided by John Hopkins University through Coursera
##
##    by: Lanny Morris
##
## Purpose of these functions are to create a 'special' matrix,
##    to solve for the inverse of the 'special' matrix, and then
##    to cache the matrix and the inverse into memory

## 'makeCacheMatrix' function creates a special "matrix" object that
##    can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## 'cacheSolve' function computes the inverse of the special "matrix" returned
##    by makeCacheMatrix.  If the inverse has already been calculated
##    (and the matrix has not changed), then the cachesolve should retrieve
##    the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
