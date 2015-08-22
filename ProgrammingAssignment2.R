## First function: returns a matrix object that can cache its inverse
##Second function: Input results from makeCacheMatrix. The function will 
##return the inversions of the matrix, either by retrieving from the cache 
##or calculating the inverse.

## Returns a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  setmatrix <- function(y){   
    x <<- y                   
    j <<- NULL                
  }
  getmatrix <- function()x  
  setsolve <- function(solve) j <<- solve
  getsolve <- function() j
  list(setmatrix = setmatrix, getmatrix = getmatrix, 
       setsolve = setsolve, getsolve = getsolve)
}             
## Takes result generated from makeCacheMatrix function and either
##retrieves its inverse from the cache or calculates the inverse.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  j <- x$getsolve()      
  if(!is.null(j)) {                 ##searches in cache for matrix
    message ("getting cached data")
    return(j)
  }
  data <- x$getmatrix()  
  j <- solve(data)       ## calculates inverse of matrix
  x$setsolve(j)       
  j                     
}