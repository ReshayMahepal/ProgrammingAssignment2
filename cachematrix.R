## The pair of functions below cache the inverse of a square matrix

## The makeCacheMatrix function creates a special matrix 
##that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y){                           ## set value of matrix
    x <<- y
    inv <<- NULL
  }                                             
  get <- function() x                           ## get values of the matrix
  set_inv <- function(inverse) inv <<- inverse  ## set value of the inverse
  get_inv <- function() inv                     ## geth value of the inverse
  list( set = set,
        get = get,
        set_inv = set_inv,
        get_inv = get_inv)
  
  
}

## cacheSolve  computes the inverse of the special matrix created 
## by the makeCacheMAtrix function. If the inverse has already been
## calculated and the matrix has not changed, the function retrieves 
## the inverse from the cache
cacheSolve <- function(x, ...) {
  ## checks if the inverse has been created and cached
  inv <- x$get_inv()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)                           ## returns cached inverse  
  }
  ## computes the inverse using the solve function
  data <- x$get()
  inv <- solve(data,...)
  x$set_inv(inv)
  print(inv)                                    ## returns the computed inverse function                       
}