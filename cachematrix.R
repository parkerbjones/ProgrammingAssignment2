## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmat <- function(matrix) m <<- matrix
        getmat <- function() m
        list(set = set, get = get,
             setmat = setmat,
             getmat = getmat)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getmat()
        if(!is.null(m)) {
                message("getting cached data")
                return(solve(m))
        }
        m <- x$get()
        x$setmat(m)
        solve(m)
}
