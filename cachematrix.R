## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special list that contains a matrix and caches its
## inverse.
##
## It takes a matrix as its argument
## Assumes that only invertible matrices are supplied
## 
makeCacheMatrix <- function(x = matrix()) {
    ## m will contain the inverse of x
    m <- NULL
    set <- function(y) {
        x <<- y
        ## Setting m to NULL guarantees updated matrices recompute their inverse
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list( set = set , get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes an object created by makeCacheMatrix
## It checks to see if the inverse of the contained matrix has been created,
## if it has it returns the cached inverse,
## if not it creates the inverse, caches it and returns it.
##
cacheSolve <- function(x, ...) {
    ## get the inverse of 'x'
    m <- x$getinverse()
    
    ## if the inverse exists, return it
    if( !is.null(m) ) {
        message("Using cached data")
        return(m)
    }
    ## else use solve to get the inverse, cache it and return it
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}