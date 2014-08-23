## The first function makes a place to store all the information we wish to save
## or not recalculate in the future. The second inputs the values into the space 
##the first function created. Please use getsolve and setsolve to interact with
## the function.

## Makes a cache for the information about the matrix so that information can 
##be retained.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(inverse) m <<- inverse
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This solves for the inverse of the square matrix and saves the information into
##the space created for it.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
}
##Hope you enjoyed it.