## The following functions provide an extended matrix object that is able to
## store the matrix itself and its inverse thanks to R's lexical scoping.

## Extend a matrix object to include get and set methods for the matrix itself
## and the corresponding inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xinv <<- inv
    getinv <- function() xinv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Working with an extended matrix object, return a cached copy of the matrix
## inverse, if available, others compute and store the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinv()
    if (!is.null(xinv)) {
        message("getting cached matrix inverse")
        return(xinv)
    }
    mat <- x$get()
    xinv <- solve(mat, ...)
    x$setinv(xinv)
    xinv
}

## The following function tests that the cached matrix and solve functions are
## working as expected

testCacheMatrix <- function(n = 4, tol = 5e-10) {
    # start with a random 4 x 4 matrix (uncached version)
    mat.unc <- matrix(rnorm(n * n), nrow = n, ncol = n)
    
    # now make the cached version
    mat.cached <- makeCacheMatrix(mat.unc)
    
    # now, find inverse (cacheSolve prints inversse but we won't save separately)
    cacheSolve(mat.cached)
    
    # test 1: is the matrix stored correctly?
    if (!identical(mat.unc, mat.cached$get())) 
        stop("matrix not stored correctly")
    
    # test 2: is the inverse as expected?
    if (!identical(solve(mat.unc), mat.cached$getinv())) 
        stop("matrix inverse not as expected")
    
    # test 3: is the inverse the actual inverse?
    A <- mat.cached$getinv() %*% mat.cached$get()  # x_inv . x = I(n) 
    B <- mat.cached$get() %*% mat.cached$getinv()  # x . x_inv = I(n)
    A.err <- sum(abs(A)) - n  # sum of absolute value of off diagonal elements
    B.err <- sum(abs(B)) - n  # sum of absolute value of off diagonal elements
    if (A.err > tol)
        stop("left inverse above tolerance")
    if (B.err > tol)
      stop("right inverse above tolerance")
}