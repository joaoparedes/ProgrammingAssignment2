## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a special 'm' object that cache's a matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set the matrix y to x and makes the inverse being NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  # set the inverse matrix to m
  setInverse <- function(inverse) m <<- inverse
  # get the inverse matrix ... can be NULL if it wasn't set previously
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
## Computes a matrix that is the inversr of 'x' retrieving it from cache if it was already computed before
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  # if a value exists return from cache
  if(!is.null(m)) {
    message("getting cached data")    
    return(m)
  }
  # get the maxtrix
  data <- x$get()
  # calculate the matrix inverse
  m <- solve(data, ...)
  # put the result on the cache
  x$setInverse(m)
  #returns the inverse matrix.
  m
}
