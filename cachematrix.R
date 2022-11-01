## makeCacheMatrix takes a matrix and performs the following functions
## get - displays the matrix
## set - sets a new matrix
## getinverse - displays the inverse matrix
## setinverse - sets the inverse. This function will be called by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function (y) {
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inv_matrix <<- inv
    getinverse <- function() inv_matrix
    list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve is a function that returns the inverse of a square matrix
## it returns NULL if the matrix is not square or if the determinant is 0
## cacheSolve makes use of a separate function makeCacheMatrix

cacheSolve <- function(x, ...) {
    inv_matrix <- x$getinverse()
    if (!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    data <- x$get()
    if (ncol(data) != nrow(data)) {
        message("matrix is not square")
        return(NULL)
    }
    if (det(data) == 0) {
        message("matrix has no inverse")
        return(NULL)
    }
    inv_matrix <- solve(data, ...)
    x$setinverse(inv_matrix)
    inv_matrix
}
