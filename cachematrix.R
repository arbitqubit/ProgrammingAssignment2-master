## This code calculates the inverse of a matrix and caches its value.
## When the function that calculates the matrix inverse is called
## again, it checks it the matrix has been updated. If the matrix is
## unchanged, it returns the cached value; if not, it recalculates the
## inverse

## makeCacheMatrix() makes a list that accepts a matrix 'x' and
## stores 'x' and its inverse in a list

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## This variable will store the inverse. Initialize it as NULL
    
    set <- function(y) {
            x <<- y           
            m <<- NULL        ## set inverse to NULL
      }
    
    get <- function() x     ## get the matrix
    
    ## store the value of inverse in the variable 'm'
    setinverse <- function(inverse) m <<- inverse
    
    ## get the value of the inverse
    getinverse <- function() m
    
    ## return the information of the matrix and its inverse as a list
    list(set=set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## This function returns the inverse of the matrix 'x'
cacheSolve <- function(x, ...) {
    m <- x$getinverse() ## get inverse
    ## If 'm' is not NULL, it means that the matrix 'x'
    ## is unchanged, so get cached value of the inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get() ## get the matrix x and store it in variable 'data'
    m <- solve(data, ...) ## calculate inverse of x
    x$setinverse(m) ## set the inverse of x
    m
}
