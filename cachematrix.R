## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix is a method that creates an object that holds 
## a Matrix and it's inverse. It initilaizes the matrix to an empty matrix 
## and it's inverse to NULL. you can set the matrix and it's inverse externally
## get and getInverse are the getter functions

makeCacheMatrix <- function(x = matrix()) {
    mInv <- NULL
    set <- function(y) {
        x <<- y
        mInv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) mInv <<- inverse
    getInverse <- function() mInv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}

## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}



