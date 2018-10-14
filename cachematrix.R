## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function is used to create a matrix and get and set its own and inverse values

makeCacheMatrix <- function(x = matrix()) {
    j <- NULL
    set <- function(z) {
       x <<-z
       j <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) j <<- inverse
    getinverse <- function() j
    list(set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## Write a short comment describing this function
## This function is used to calculate the inverse of the matrix.
## If it has already been calculated, the function should get the data from the cache

cacheSolve <- function(x, ...) {
    j <- x$getinverse()
    if  (!is.null(j)) {
        message("getting data from cache")
        return(j)
    }
    data <- x$get()
    j <- solve(data, ...)
    x$setinverse(j)
    j
}
