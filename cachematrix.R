## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        zone <- NULL
        set <- function(y) {
                x <<- y
                zone <<- NULL
        }
        get <- function() x
        setInv <- function(inv) zone <<- inv
        getInv <- function() zone
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        zone <- x$getInv()
        if(!is.null(zone)) {
                message("getting cached data")
                return(zone)
        }
        dt <- x$get()
        zone <- solve(dt, ...)
        x$setInv(zone)
        zone
}
