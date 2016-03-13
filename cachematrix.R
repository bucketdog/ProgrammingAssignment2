## Create a list to (1) set the value of a matrix, (2) get the value of a  
## matrix, (3) set the value of its inverse, (4) get the value of its inverse  

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get, setsolve = setsolve, 
         getsolve = getsolve)
}


## Check the cache to see if the inverse has been calculated. If so, retrieve
## inverse from the cache. If not, calculate inverse.

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
