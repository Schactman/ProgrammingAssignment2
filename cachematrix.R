## Invert a matrix. 
## If the inverse exists in a cache, then use that.
## Otherwise compute the inverse.
##
## if A is a matrix, the assign AA <- makeCacheMatrix(A)
## call cachesolve(AA) to invert A matrix.

## makeCacheMatrix is a special vector
## containing a list of functions

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinv <- function(inv) m <<- inv
   getinv <- function() m
   
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
   
}


## Return the inverse of a matrix
## Either newly computed or using the cached value

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   m <- x$getinv()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data)
   x$setinv(m)
   m
}
