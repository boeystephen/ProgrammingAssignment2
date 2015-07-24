## Matrix Inversion is usually a costly computation
## The two functions below caches the inverse of a matrix rather than compute it repeatedly 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve function will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
	## Test for a square matrix
    if (nrow(x$get()) != ncol(x$get())) {
        message("Not possible to invert a rectangular matrix")
        return()
    }
    ## Return a matrix that is the inverse of 'x'
		    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)    
    x$setinv(i)
    i
}
