## These functions are used to creeate an onject that stores a matrix and caches its
## inverse.  This enables a fast inverse calculation for large matrices.  This code assumes
## that the matrix input by the user is invertible.  However a check is provided that maybe 
## used by uncommenting if needed. 

## This function, does the following:
## creates a list to be used in cacheSolve().  The list contains a function to
## 1. set the matrix, 2. get the matrix, 3. set the inverse,  4. get the inverse



makeCacheMatrix <- function(M = matrix()) {
  
  ## check for invertibitliy, May be used if desired 
  ## Mdim = dim(M)  ## matrix dimension 
  ## if (Mdim[1] != Mdim[2]){
  ##  print("Error: Matrix is not Square, Try Again")
  ##  return() } else if (det(M) == 0) { 
  ##    print("Error: Matrix is not invertible, Try Again")
  ##    return()
  ##  }
  
  
  Minv <- NULL
  set<- function(y) {
    
    M<<- y
    Minv <<- NULL
  }
  get <- function() M
  setinv <- function(inverse) Minv <<- inverse
  getinv <- function() Minv
  
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
  
  ##print(M) 
  
}


## This function calculates the inverse of the Matrix created above.  It first checks if the 
## inverse has already been calculated, if true it gets the inverse from the cache and skips
## the computation, else the inverse is calculated.

cacheSolve <- function(M, ...) {
  ## Return a matrix that is the inverse of 'M'
  
  Minv <- M$getinv()
  
  if(!is.null(Minv)) {
    message("getting cached data")
    return(Minv)
  }
  data <- M$get()
  Minv <- solve(data,...)
  M$setinv(Minv)
  return(Minv)
  
  ##print(Minv)
  
}
