## The two following functions create a matrix 
## capable of storing the value of its inverse
## (after the first time it is calculated)
## and recalling it instead of recalculating it.


## The first function, makeVector creates a special "matrix",
## which is really a list containing a function
## to set and get the value of the matrix and its inverse.

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


## The following function calculates the inverse of the matrix
## created with the above "makeCacheMatrix" function.
## It first checks to see if the inverse has already been calculated.
## If it has, it gets the inverse from the cache and skips the computation.
## If it hasn't, it calculates the inverse of the data and
## sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    m
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
