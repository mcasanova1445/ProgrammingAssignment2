## These functions implement a special kind
## of matrix that caches its inverse

## This function create the special matrices
## and returns a list of functions to get and
## set its values and its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
            x <<- y
            s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function calculates and caches the inverse
## of a special matrix, or takes the cached value in
## case it already exists

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
            message("getting cached data")
            return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
