## function makeCacheMatrix allows a user to create 
## a matrix which is then passed to the function cacheSolve
## in order to create an inverse of the abovementioned matrix
## and to store it in variable getInv using it as cache. 
## if cacheSolve is called when the getInv already contains the 
##inverse of the matrix from the makeCacheMatrix function then
##it does not compute the inverse, but retreives it from stored 
##getInv variable

## the following function (makeCacheMatrix) allows a user to create 
## a matrix which is then passed to the function cacheSolve

makeCacheMatrix <- function(x=matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## the following function (cacheSolve) if used for the first time 
## takes the matrix from the function makeCacheMatrix, 
## computes an inverse of this matrix
## and stores it in variable getInv using it as cache. 
## if cacheSolve is called when the getInv already contains the 
## inverse of the matrix from the makeCacheMatrix function then
## it does not compute the inverse, but retreives it from stored 
## getInv variable

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}