## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # returns a list of functions for:
    # - setting the matrix x
    # - getting the matrix x
    # - setting the inverse of the matrix x (s)
    # - getting the inverse of the matrix x
    s <- NULL
    set <- function(y) {
            x <<- y
            s <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) s <<- inverse
    getinverse <- function() s
    list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse()
    # return cached inverse if it exists
    if(!is.null(s)){
        message("getting cached inverse")
        return(s)
    }
    # otherwise call solve to get the inverse and set it
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}
