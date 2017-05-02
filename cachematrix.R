## makeCacheMatrix creates a special matrix object which is a list containing functions to do the following :
	## 1. Sets the matrix
	## 2. Gets the matrix
	## 3. Sets the inverse of the matrix
	## 4. Gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
# Set the matrix globall and it's inverse to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
# Get the matrix
  get <- function() x
# Set the inverse matrix
  setinv <- function(i) m <<- i
# Get the inverse matrix
  getinv <- function() m
# Return the list of functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve calculates the inverse of the special matrix created from makeCacheMatrix.  It first checks to see if the inverse already exists if so then return and skip the calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
# Get the inverse matrix
  m <- x$getinv()
# Check if inverse matrix already exists, if so then return and do nothing
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
# Inverse does not exist so create the matrix object
  data <- x$get()
# Calculate the inverse using the solve function
  m <- solve(data, ...)
# Set the inverse global to the inverse function m
  x$setinv(m)
# show the inverse matrix
  m

}


