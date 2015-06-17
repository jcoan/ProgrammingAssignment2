## The following two functions allow for a user-inputted inversible matrix to calculate & cache
## its own inverse so that the inverse does not have to be calculated repeatedly, saving on
## computational time.

## makeCacheMatrix sets up the user-inputted matrix as an object that can cache its own inverse,
## but does not yet calculate the inverse at this point.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve checks to see if the inverse of the user-inputted matrix has already been cached,
## and if it has the inverse is returned from the cache. If the inverse has not yet been cached,
## the function calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
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
