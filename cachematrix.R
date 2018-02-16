## Irene Chen
## Feb 2017, R Programming Coursera

## The makeCacheMatrix() and cacheSolve() 
## functions create an R matrix object 
## and store the inverse of that matrix

## makeCacheMatrix() creates a matrix object
## where the inverse can be cached 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() computes the inverse of the
## object returned by makeCacheMatrix. If the
## matrix inverse has already been cached, then
## cacheSolve retrieves (and does not compute)
## the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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