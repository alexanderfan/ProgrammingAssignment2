## This script contains two functions which can be used to
## create a cache Matrix and set the inverse matrix value
## of the matrix.

## This function creates a cache matrix object that has a few
## sub functions associated to the:
##  1. Setting the matrix value
##  2. Returning the matrix value
##  3. Setting the inverse matrix value
##  4. Returning the inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Sets the value of the matrix
  set <- function(inputMatrix) {
    x <<- inputMatrix
    m <<- NULL
  }
  
  ## Returns the value of the matrix
  get <- function() x
  
  ## Sets the inverse of the matrix
  setinv <- function(invmatrix = matrix()) inv <<- invmatrix
  
  ## Returns the value of the inverse matrix
  getinv <- function() inv
  
  ## Create a list with the functions in array
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function either calculates the inverse matrix or uses the cached
## if it was previously calculated.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  ## If the inverse has already been cached, don't need to recalculate it
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Solve for the inverse matrix using the solve() function
  inv <- solve(x$get())
  
  ## Set the inverse matrix into the cacheMatrix
  x$setinv(inv)
  
  ##Print the inverse matrix to the screen
  inv
}