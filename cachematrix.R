## 20171207 - Nate Caldwell
## These functions are for caching matrix inverses into memory
## prior to conducting computation.  This allows for the original matrix,
## the function, and the solution all to remain within memory for
## swifter access.

## The makeCacheMatrix function holds matrix (x) in memory, while
## also holding all associated functions for a quick call.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsol <- function(solve) i <<- solve
  getsol <- function() i
  list(set = set, get = get,
       setsol = setsol,
       getsol = getsol)
}

## cacheSolve pulls the previously cached functions from makeCacheMatrix
## and enables us to quickly pull the original matrix and all functions 
## from memory.

cacheSolve <- function(x, ...) {
  i <- x$getsol()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsol(i)
  i
}

