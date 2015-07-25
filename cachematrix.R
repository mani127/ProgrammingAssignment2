## Matrix inversion is usually a time-consuming computation. For a very big matrix, there
## is a benefit in caching the inverse of the matrix instead of computing it repeatedly. The 
## following two functions compute and cache the inverse of a matrix.


## makeCacheMatrix function creates a special "matrix" object that cache its inverse.
## This function takes an invertible matrix as input and returns a list containing 
## function to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
## This list is passed as an input to cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
 inv = NULL
 
 set = function(y) {
   x <<- y
   inv <<- NULL
 }
 
 get = function() x
 setinv = function(inverse) inv <<- inverse
 getinv = function() inv
 list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function returns the inverse of an invertible matrix. The first step is to check
## whether the inverse has already been computed and if computed, it retrieves the result and
## skips the computation. If not computed, it computes the inverse and sets the value in cahce
## via setinv fucntion.
## This function takes output of makeCacheMatrix and returns the inverse of the matrix input
## to makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  ## if the inverse has already been computed
  if(!is.null(inv)) {
    ## fetch it from cache and skip the computation 
    message("getting data from cache")
    return(inv)
  }
  
  ## else compute the inverse
  data = x$get()
  inv = solve(data)
  
  x$setinv(inv)
  
  return(inv)
}