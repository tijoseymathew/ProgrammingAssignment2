## Caches a matrix and its inverse to avoid repeated recalculations
# Create a cached matrix object of an existing matrix 'mat' using
#> cacheMat <- makeCacheMatrix(mat)
# Get the inverse using
#> invMat <- cacheSolve(cacheMat)

## Creates an instance of matrix and inverse and returns a list with following methods
#obj$set(m)       : Set a new cached matrix
#obj$get()        : Returns the cached matrix
#obj$setInverse(m): Sets the inverse of cached matrix
#obj$getInverse() : Returns the cached inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(m){
    x <<- m
    #Inverse is not calculated here to avoid unnecessary first time computation
    inv_x <<- NULL
  }
  get <- function() x
  setInverse <- function(m) inv_x <<- m
  getInverse <- function() inv_x
  #Return a list of methods on this instance
  list( set=set, get=get, 
        setInverse=setInverse, getInverse=getInverse)
}


## Returns the inverse of an instance of makeCacheMatrix
# If matrix inverse exists, it is not recomputed.
#>cacheSolve(cacheMat)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)){
    #Inverse is already present just return it!
    message("Getting cached inverse")
    return(inv)
  }
  #Inverse does not exist, compute, save and return it!
  message("Computing inverse")
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
