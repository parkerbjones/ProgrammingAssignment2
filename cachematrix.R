## Creates an object for storing the raw data matrix and defines object properties for
## storing the matrix's inverse

## Used example matrix from ?solve
## hilbert <- function(n) { i <- 1:n; 1 / outer(i-1,i,"+")}
## h8 <- hilbert(8)
## a <- makeCacheMatrix(h8)
## cacheSolve(a)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmat <- function(solve) m <<- solve
        getmat <- function() m
        list(set = set, get = get,
             setmat = setmat,
             getmat = getmat)
}


## If we have not calculated the matrix's inverse before, cacheSolve retrieves the raw data
## and uses Solve(). The matrix's inverse is then stored for retrieval later if cacheSolve()
## is used again.

cacheSolve <- function(x, ...) {
        m <- x$getmat()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmat(m)
        m
}
