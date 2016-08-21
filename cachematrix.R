## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse
## of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here).
## These pair of functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  # Initialiazation of inverse as null
  inverse <- NULL
  
  # set value of matrix
  set <- function(newMatrix) {
    inverse <<- NULL
    x <<- newMatrix
  }
  
  # get the matrix
  get <- function() {
    x
  }
  
  # set inverse matrix
  setInverse <- function(inv) {
    inverse <<-inv
  }
  
  # gets the inverse matrix
  getInverse <- function() {
    inverse
  }
  
  # The list with available functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getmean)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  res <- x$getInverse()
  
  #checks result for value to return or computes that value
  if(!is.null(res)){
    return(res)
  }
  else {
    #solve for x invese and set it 
    res <- solve(x$get(), ...)
    x$setInverse(res)
    
    #return the result
    res
  }
}
