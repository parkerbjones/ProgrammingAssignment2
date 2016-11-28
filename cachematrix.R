## Creates an object storing our original matrix
## Used example matrix from ?solve
## hilbert <- function(n) { i <- 1:n; 1 / outer(i-1,i,"+")}
## h8 <- hilbert(8)
## a <- makeCacheMatrix(h8)
## cacheSolve(a)

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


## If m is null, then we will calculate the inverse of our matrix
## If m is not null, then we will return the previously calculated inverse of our matrix

cacheSolve <- function(x, ...) {
        m <- x$getmat()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmat(m)
        m
}
