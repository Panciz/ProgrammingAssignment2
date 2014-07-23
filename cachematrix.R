## Put comments here that give an overall description of what your
## functions do

## Returns a special object  that can store the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       getInverse = getInverse,
       setInverse = setInverse)
}


## Calculates the inverse of the special object created with the makeCacheMatrix function.
## If the inverse ha already been calculate and the matrix contained in the object did not change 
## the cached value is returned.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
