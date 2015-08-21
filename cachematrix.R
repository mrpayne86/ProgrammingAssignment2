## The following set of functions utilise scoping rules of the R language to
## compute the inverse of a matrix.


## The following function creates a special "matrix", which is really a list
## containing a function to:
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse
##   4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    
    ## function to set the matrix
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    
    ## function to get the matrix
    get <- function() x
    
    ## function to set the inverse matrix
    setinv <- function(inv) x_inv <<- inv
    
    ## function to get the inverse matrix
    getinv <- function() x_inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" created
## with the above function. It first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the matrix and sets the
## value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    ## get the cached value of the inverse matrix
    x_inv <- x$getinv()
    
    ## if the inverse matrix has been previously computed, then return its value
    if(!is.null(x_inv)) {
        message("getting cached data")
        return(x_inv)
    }
    
    ## if the inverse matrix has not been previously computed, then compute its
    ## value
    data <- x$get()
    x_inv <- solve(data, ...)
    
    ## store computed inverse matrix
    x$setinv(x_inv)
    
    ## return inverse value
    x_inv
}
