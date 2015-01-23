## makeCacheMatrix creates a special object that stores a matrix and caches's the inverse.
## This special object is a list containing the following functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
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
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
