## makecachematrix creates a matrix that can cache its inverse
## cachesolve computes the inverse of a matrix

## returns a matrix with functions to get & set value and get & set inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    ## getter and setter for matrix
    get <- function() x
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## getter and setter for the inverse matrix
    getinverse <- function() inverse
    setinverse <- function(inv) inverse <<- inv

    list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


## returns the inverse of the matrix. cached inverse is returned if inverse
## has been calculated before

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("cached inverse")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m, ...)
    x$setinv(inv)
    return(inv)
}
