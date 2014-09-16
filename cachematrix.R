## Please note:
## * assumes that the matrix supplied is always invertible. **

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
##   and return the cached result if applicable.
##   * by default `solve` with only a single matrix argument will 
##   solve for x in:  a %*% x = b ; where b=Iₙ (the identity matrix).
##   * this function does cache the full `solve` call,
##   and this may be useful, but be sure to re-set or initialize the matrix 
##   with `makeCacheMatrix` if you change arguments to `solve`
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

# m<-makeCacheMatrix(matrix(1:4,2,2))
# cacheSolve(m)
# cacheSolve(m)
# m$set(matrix(c(1,2,3,6,5,4,9,7,8), 3, 3), )
# cacheSolve(m)
# cacheSolve(m)
# m$get() %*% cacheSolve(m)