## cachematrix.R
##
## Bob George  https://github.com/rwgeorge
###############################################################################
## Coursera R Programming (rprog-011)
## Assignment 2: Caching the Inverse of a Matrix
## https://github.com/rwgeorge/ProgrammingAssignment2
## 
## Description: Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than compute it
## repeatedly (there are also alternatives to matrix inversion that we will not
## discuss here). Your assignment is to write a pair of functions that cache
## the inverse of a matrix.
##
## Write the following functions:
## 
## 1. makeCacheMatrix: This function creates a special "matrix" object that
##    can cache its inverse.
##
## 2. cacheSolve: This function computes the inverse of the special "matrix"
##    returned by makeCacheMatrix above. If the inverse has already been 
##    calculated (and the matrix has not changed), then the cachesolve should
##    retrieve the inverse from the cache.
###############################################################################



## This function creates a special "matrix" object that can cache its inverse.
## It is assumed that the supplied matrix will always be invertible.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.  
## It is assumed that the supplied matrix will always be invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("Getting cached data...")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}