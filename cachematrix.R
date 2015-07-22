## Cache potentially time-consuming computations

## Assign computation to an object in an environment that is different from the current environment = cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Check to see if the computation has already been calculated 
## If so, it gets the value from the cache and skips the computation 
## Otherwise, it performs calculation and creates a cache of this calculation

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
