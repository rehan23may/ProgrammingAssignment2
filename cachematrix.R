## Put comments here that give an overall description of what your
## functions do
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      rk <- NULL
      set <- function(y) {
            x <<- y
            rk <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) rk <<- solve
      getinverse <- function() rk
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		      rk <- x$getinverse()
      if(!is.null(rk)) {
            message("getting cached data")
            return(rk)
      }
      data <- x$get()
      rk <- solve(data, ...)
      x$setinverse(rk)
      rk
}
