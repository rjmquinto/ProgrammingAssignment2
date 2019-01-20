## Put comments here that give an overall description of what your
## functions do

# The enclosure of inverse matrix cache
makeCacheMatrix <- function(x = matrix()) {
    mat_inv <- NULL
    set <- function(y) {
        x <<- y
        mat_inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(inv) mat_inv <<- inv
    getinv <- function() mat_inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
    mat_inv <- x$getinv()
    if(!is.null(mat_inv))
        return(mat_inv)
    
    mat_inv <- solve(x$get(), ...)
    x$setinv(mat_inv)
    mat_inv
}
