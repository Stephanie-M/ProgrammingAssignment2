## Creates a cached matrix that will used to compute 
## the inverse of the matrix

## Function creates cached matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Function checks cache for inverse of matrix, if found 
##returns cached matrix, if not computes inverse of x
cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  ## Checks if inverse matrix is cached
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## matrix not found, computes inverse
  data <- x$get()
  i <- solve(data) %*% data
  x$setsolve(i)
  i
}
