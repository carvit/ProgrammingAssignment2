## makeCacheMatrix is a function, which creates
##    a special list object with following of functions
##      set(x) - sets the matrix object
##      get() - gets the matrix object
##      setinv(inv) - sets the inverse of the matrix
##      getinv() - gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve is a function, which returns the inverse of the matrix
## parameter x is supposed to be an object created by the makeCacheMatrix function
## inverse of the matrix is created by the solve function only if no cached inverse matrix exists

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
