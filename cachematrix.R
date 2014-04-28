###############################################################################
## The makeCacheMatrix function create a category of special 'matrix'. It 
## outputs four functions that allows us to view and manipulate the content of 
## the special 'matrix' and associate information (i.e., its inverse)
################################################################################

makeCacheMatrix <- function(x = matrix()) {
  # Create a new "special" matrix inverse and initialize its inverse to NULL
  inverse.x  <- NULL
  
  # Function to set the special "matrix" to a new value and 
  # re-initialize inverse.x to NULL
  set <- function(y){
    x <<- y
    inverse.x <<- NULL
  }
  
  # Function get the value of the matrix 
  get <- function() {
    x
  }
  
  # Function to set the inverse of the matrix 
  setInverse <- function(already.computed.inverse) {
    inverse.x <<- already.computed.inverse 
  }
  
  # Function to get the current value of the matrix's inverse
  getInverse <- function() {
    inverse.x 
  }
  
  # Return the list of functions (associated to the 'special' matrix)
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


##############################################################################
## The cacheSolve function is client function that uses "makeCacheMatrix" 
## function in its implementation to cache the computation of the inverse of 
## a 'special' matrix 
##############################################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # First get the 'status' the inverse of the special 'matrix'  
  inverse.x <- x$getInverse()
  
  # If is it not NULL, it is already computed and we only need to return it
  if (!is.null(inverse.x)){
    message("Getting cached inverse")
    return(inverse.x)
  }
  # otherwise compute it here
  data <- x$get()
  inverse.x <- solve(data, ...)
  x$setInverse(inverse.x)
  inverse.x
}


