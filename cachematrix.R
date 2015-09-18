## Put comments here that give an overall description of what your
## functions do
## 
## Write a short comment describing this function

# makeCacheMatrix takes a matrix (assumed invertible) as input
# and outputs a list of four functions (set, get, setinverse, getinverse)
# set permits values of the matrix to be altered/updated
# get returns current value of the matrix
# setinverse provides updated values of the inverse (as calculated by cacheSolve)
# getinverse returns current value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

# cacheSolve calculates the inverse of a matrix. It takes the output of 
# makeCacheMatrix as an input and returns the inverse of an matrix.
# First, it checks whether this has been computed previously (via the makeCacheMatrix
# object, ie getinverse function). If this is a non-NULL value, it returns this value.
# If getinverse is NULL, the matrix is accessed, the inverse is computed, and passed to makeCacheMatrix
# to be stored in getinverse (i.e. cached). The inverse is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}