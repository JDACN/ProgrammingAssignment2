## Caching the Inverse of a Matrix:

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## makeCacheMatrix : This function is a function that returns a list of functions
## Its puspose is to store a martix and a cached value
## - set            set the value of a matrix
## - get            get the value of a matrix
## - setInverse     set the cahced value (inverse of the matrix)
## - getInverse     get the cahced value (inverse of the matrix)

##############################################################
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL                             # initially nothing is cached so set it to NULL
  
  # store a matrix
  set <- function(y) {
    x <<- y                             # assign new value
    i <<- NULL                          # since matrix has new value clear the cache
  }
  
  # returns the stored matrix
  get <- function(){ x }
  
  # set the cache 
  setinverse <- function(inverse){
    i <<- inverse                      # set input value to cache
  }
  
  # get the cache
  getinverse <- function(){ 
    i                                  # return cache value
  }
  
  # return a list.
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)

}


## The following cachesolve calculates the inverse of a "special" matrix created with makeCacheMatrix
## This Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

  i <- x$getinverse()                  # try to get cached matrix
 
   if(!is.null(i)) {                   # success return cached value
    message("getting cached data")
    return(i)
   }
  
  data <- x$get()                      # get Matrix as no cache
  i <- solve(data, ...)                # inverse it
  x$setinverse(i)                      # cache it 
  i                                    # return
}
