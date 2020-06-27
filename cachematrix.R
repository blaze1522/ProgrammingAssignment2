## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## ## This function creates a special vector that caches the calculation of its inverse
## It does this by creating the object as a list with `set` and `get` functions.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x 
        setinv <- function(inverse) s <<- inverse
        getinve <- function() s 
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This function will calculate the inverse of the received square matrix, caching
## the result in the CacheMatrix object.
## In the cache already exists, it will simply return the cached result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinv()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$setinv(s)
        s
}
