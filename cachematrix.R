## The two functions viz., makeCacheMatrix and cacheSolve defined in this script
## provide means for making time-efficient reuse of matrix inverse, by avoiding
## unnecessary computations everytime it is required, by managing the relevant data on a Cache. 
## The makeCachematrix function creates the necessary 'matrix' object and,
## the cachesolve function computes the inverse, only if a Cached copy doesn't exist


makeCacheMatrix <- function(x = matrix()) {
  # Creates a matrix object 'x', provides functions for storing & retrieving, 
  # 'x' and its inverse from the Cache
  # 
  # Args:
  #   x: a data matrix
  #   
  # Returns:
  #   A CacheMatrix object is returned. It contains the following functions -
  #
  #   set()     : Sets the new input data and clears the matrix inverse
  #   get()     : retrieves the original data matrix 'x'
  #   setminv() : sets the matrix inverse into the Cache
  #   getminv() : retries the matrix inverse from the Cache
  
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  setminv <- function(inv_m) m_inv <<- inv_m
  getminv <- function() m_inv
  list(set = set, get = get,
       setminv = setminv,
       getminv = getminv)
}



cacheSolve <- function(x, verbose=FALSE, ...) {
  # Computes and returns the matrix inverse of 'x', 
  # 
  # Args:
  #   x: a special 'matrix' object of the type created by the function 'makeCacheMatrix'
  #   
  #   verbose: If TRUE, prints the matrix inverse; if not, not. Default is FALSE.
  #
  # Returns:
  #   The inverse of the matrix 'x' is retrieved from the cache, if a copy exists in it. 
  #   If it doesn't, inverse is computed, provided it exists. 
  #   For a singular x, an error would be returned.
  
  m <- x$getminv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setminv(m)
  if (verbose)
    cat("Inverse = ", round(m, 4), ".\n", sep = "\t")
  
  invisible(m)
}
