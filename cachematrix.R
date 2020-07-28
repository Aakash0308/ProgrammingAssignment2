## The purpose is for caching the inverse of a matrix
## There are 2 functions that are mentioned below


## Together they are used to create a special matrix
## and store it and caches its inverse

## This function creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  i <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    x <<- matrix
    i <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() {
    ## Return the matrix from function
    x
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return a list of the methods
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This fuction is used for finding the inverse of the special matrix
## created using the above function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  mat <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(mat, ...)
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
