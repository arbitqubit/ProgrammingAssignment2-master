## This code calculates and caches the inverse of a matrix. If the matrix is unchanged when the function is called again, it returns the cached value of inverse. If not, it recalculates the inverse.


## makeCacheMatrix() accepts a matrix 'x' and stores the matrix and its inverse in a
## list

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)

}


## cacheSolve() returns the inverse of the matrix 'x'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
