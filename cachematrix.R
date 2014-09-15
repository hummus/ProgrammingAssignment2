## Please note:
## * assume that the matrix supplied is always invertible. **

# creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setsolve <- function(solved) m <<- solved
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## compute the inverse of the special "matrix" returned by makeCacheMatrix
##   and return the cached result if applicable
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ##   (inverse already calculated & matrix unchanged) -> retrieve from cache
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

m0 <- cbind(c(0,0,1), c(0,1,0), c(1,0,0))
m <- makeCacheMatrix(m0)
mi <- cacheSolve(m)
m$get()
mi
m0%*%mi