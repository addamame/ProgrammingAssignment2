## Utility functions that let you cache the inverse of a matrix

## Returns an object that stores the matrix argument, provides utility methods to retrieve it,
## and methods to get/set its inverse
## When used in conjunction with its brother utility method "cacheSolve", provides a handy
## mechanism to cache the inverse of the argument matrix

makeCacheMatrix <- function(mtrx = matrix()) {
   inv_mtrx <- NULL
   set <- function(y) {
      mtrx <<- y
      inv_mtrx <<- NULL
   }
   get <- function() mtrx
   setInverse <- function(matrix_inverse) inv_mtrx <<- matrix_inverse
   getInverse <- function() inv_mtrx
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Meant as a replacement to the solve() function, and to provide caching functionality for
## the output of the solve() function. Takes as an argument the object created by makeCacheMatrix()
## and returns the inverse - caching the value once it has been calculated

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   inv <- x$getInverse()
   if (!is.null(inv))
   {
      message("getting inverse from cache")
      return(inv)
   }
   mtrx <- x$get()
   inv <- solve(mtrx, ...)
   x$setInverse(inv)
   inv
}
