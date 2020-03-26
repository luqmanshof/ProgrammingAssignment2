## Put comments here that give an overall description of what your
## functions do
## The overall description of what the function do is to enable the caching of the inverse of matrix
## thereby preventing it from being repeatedly computed whenever it is needed


## Write a short comment describing this function
## The makeCacheMatrix function creates a special matrix function that
## that can cache its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    setMat <- function(y) {    ## Assigns new value of matrix
        x <<- y
        invMat <<- NULL
    }
    getMat <- function() x     ## gets the value of the matrix
    
    setInverse <- function(inverse) invMat <<- inverse  ## set the value of the invertible matrix
    getInverse <- function() invMat                     ## get the value of the invertible matrix
    
    list(setMat = setMat, getMat = getMat,
         setInverse = setInverse, getInverse = getInverse)
    
}


## Write a short comment describing this function
## This function computes the inverse of the "matrix" returned by makeCacheMatrix above.
## The cacheSolve function first check if the inverse has already been calculated 
## and stored in the cache, then it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    invMat <- getInverse    ## gets the value of the invertible matrix from makeCacheMatrix
    
    if(!is.null(invMat)) {                  ## if inverse matrix is not NULL
        message("Getting Cached Matrix")    ## Type message: Getting Cached Matrix 
        return(invMat)                      ## return the invertible matrix
    }
    
    #if value of the invMat is NULL then  
    
    MatData <- x$getMat()           ## get the original Matrix Data 
    invMat <- solve(MatData, ...)   ## use solve function to inverse the matrix
    x$setInverse(invMat)            ## set the invertible matrix 
    invMat                          ## return the invertible matrix
}
