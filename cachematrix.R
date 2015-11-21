## Put comments here that give an overall description of what your
## functions do
#
# Matrix Inversion
# This R program contains functions that perform calculates of an inverse matrix.
# The matrix and inverse matrix are cached for repeatly usage.
#

#
# makeCacheMatrix proivdes a list of functions to store and retrieve matrix and inverse matrix
#
library(matrixcalc)
makeCacheMatrix <- function(x = matrix()) {
# check in x is a square matrix
        if (!is.square.matrix(x))
                stop("Input is not a sqaure matrix")
        
        mx <- NULL
        set <- function(y) {
                x <<- y
                mx <<- NULL
        }
        get <- function() x
        
        setinverse <- function(minv) mx <<- minv
        getinverse <- function() mx
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#
# cacheSolve returns inverse of a makeCacheMatrix object.
# If an inverse already cached, it returns the cached inverse matrix.
# Otherwise, it calculates the inverse matrix, caches it and returns it.
#

cacheSolve <- function(x, ...) {
        mx <- x$getinverse()
        if(!is.null(mx)) {
                message("getting cached inverse matrix...")
                return(mx)
        }
        ## Return a matrix that is the inverse of 'x'
        org <- x$get()
        mx <- solve(org)
        x$setinverse(mx)
        mx
}
