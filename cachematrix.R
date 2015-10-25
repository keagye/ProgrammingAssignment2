## This pair of functions creates a way to store the
## inverse of a matrix without having to recompute it

## makeCacheMatrix creates and holds an input matrix for future use,
## callable by another function
## For an input matrix "z", z$get will return the matrix,
## and z$getinverse will return NULL because it is set to NULL in
## makeCacheMatrix's environment via the superassign operator (<<-)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve returns the inverse of an input matrix via the callable
## variables superassigned in makeCacheMatrix;
## The first execution of cacheSolve will compute the inverse;
## later executions will return the cached inverse
## Modifying the existing matrix using z$set will reset the input matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
