## makeCacheMatrix creates a special object that stores a matrix and caches's the inverse.
## This special object is a list containing the following functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y           # store the new matrix.
            m <<- NULL        # clear the previous cached inverse of the old matrix.
      }
      get <- function() x
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve calculates the inverse of the special "matrix" created by makeCacheMatrix.
## It first checks to see if the inverse has already been calculated. If so, it gets the
## inverse from the cache and skips the computation. Otherwise, it calculates the inverse
## of the m and sets the value in the cache via the setinv function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()         # retrieve the cached inverse matrix (if available). 
      if(!is.null(m)) {       # Not NULL means that a cached inverse matrix is available.
            message("getting cached data")
            return(m)
      }
      data <- x$get()         # get the matrix that is stored.
      m <- solve(data, ...)   # solve() is the R function that returns the inverse of a square matrix.
      x$setinv(m)             # cache the inverse matrix so that we can reuse it later.
      m
}
