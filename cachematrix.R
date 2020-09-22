## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize
  m <- NULL
  ## Method to set the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  ## Method to get the matrix
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m 
  ## List of methods
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## Return if not null
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## Calculate
  m <- solve(data,...)
  ## Set the inverse to the object
  x$setInverse(m)
  ## Return
  m
}
