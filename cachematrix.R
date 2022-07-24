## The following 2 functions allow to calculate the inverse of a matrix which 
## is subsequenly stored in cache
## This function returns a list of 4 functions:
## - for setting/getting values of a matrix
## - for setting/getting the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function calculates the inverse of the special “matrix” created with the above function.
## It first checks to see if the inverse has already been calculated. If then it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets it in the cache via the setinverse function.

cacheinverse <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}
