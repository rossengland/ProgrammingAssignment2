## The makeCacheMatrix and cachesolve functions will work together such that
## matrix inverse calculated by the CacheSolve function will be stored in cache
## by the makeCacheMatrix function. If CacheSolve is called on a matrix that has 
## already been previously solved, it will return the inverse value previously
## cached by the makeCacheMatrix function.

## makeCacheMatrix will function to store matrix inverses newly calculated
## by CacheSolve OR to return the stored values of matrices that CacheSolve
## has been called on repeatedly

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## i will be the inverse of matrix x; reset to null on repeat calls
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  getm <- function() x ## returns value of matrix x
  setinv <- function(solve) i <<- solve ## will store inverse of x in cache
  getinv <- function() i ## returns inverse from stored value in cache
  list(set = set, getm = getm, setinv = setinv, getinv = getinv)
}


## CacheSolve will will, when first called on matrix, solve the inverse
## and return it to the console and store the inverse in the cache using the
## makeCacheMatrix function. When called again on the same matrix, it will use the 
## makeCacheMatrix function to return the cached value of the inverse

cacheSolve <- function(x, ...) {
  
        i <- x$getinv() ## Return a matrix that is the inverse of 'x'
        
        if(!is.null(i)) { 
          message("getting cached data") ##returns cached i if i is not NULL
          return(i)
        }
        
        v <- x$getm() ##if i was NULL above, assigns matrix x to data
        i <- solve(v , ...) ##solves inverse of matrix and stores to i
        x$setinv(i) ##stores value of inverse in cache
        i ## returns value of inverse to console
}
