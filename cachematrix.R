## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(x) {
        m <<- x
        i <<- NULL
    }
    get <- function() m
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above

cacheSolve <- function(m, ...) {
    i <- m$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    matrix <- m$get()
    i <- solve(matrix, ...)
    m$setinverse(i)
    i
}

