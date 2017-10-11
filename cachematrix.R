## Pair of functions that cache the Inverse of a Matrix: 
## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      ## <<- to assign a value to an object in an environment 
      ## different from the current environment
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
      ## if the inverse has already been calculated, get it 
      ## from the cache and skip computation
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
      ## else, get the inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
