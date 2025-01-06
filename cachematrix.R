## makeCacheMatrix creates a matrix object x that can cache its inverse
## cacheSolve will return the inverse of a matrix x by either solving for it or returning it from the cache (if already solved)

## makeCacheMatrix creates a matrix object x that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ##create function makeCacheMatrix, only argument is matrix x
  i <- NULL                                 ##create an object i to cache the inverse of the matrix, set to NULL initially since the function hasn't calculated the inverse yet
  set <- function(y) {                      ## create function set which will take argument y and cache it as object x and cache a NULL value to object m
    x <<- y
    i <<- NULL
  }
  get <- function() x                       ##create function get, which returns the value of object x
  setinv <- function(solve) i <<- solve     ##create function setinv, which will cache the inverse of the matrix
  getinv <- function() i                    ##create function getinv, which will return the cached inverse of the matrix
  list(set = set, get = get,                ## create a list consisting of the 4 functions created inside the function cacheMatrix
       setinv = setinv,
       getinv = getinv)
}








## cacheSolve will return the inverse of a matrix x by either solving for it or returning it from the cache (if already solved)

cacheSolve <- function(x, ...) {
  i <- x$getinv()                      #stores previously cached i value in object i
  if(!is.null(i)) {                    #checks that the previous i value is not NULL
    message("getting cached data")     #if not NULL, prints "getting cached data"
    return(i)                          #prints value of inverse matrix i
  }
  data <- x$get()                     #if i was NULL, creates object data containing stored matrix x
  i <- solve(data, ...)               #solves inverse of matrix x/data and stores to object i
  x$setinv(i)
  i
}