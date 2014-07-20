## These two functions create and mantain an special object to store a matrix and 
## cache its inverse. In addition, there is other function to calculate the inverse
## of the matrix or return the cached matrix if calculated.

## Create a new object CacheMatrix
## containing a matrix and necesary functions to use it:
## get <<- Returns the data matrix
## set <<- Set the data matrix
## getinverse <<- If calculated, returns the inverse of the data matrix
## setinverse <<- Set the inverse of the data matrix

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


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
