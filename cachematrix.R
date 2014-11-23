##=================================================================
# Author: Brandon Chiazza
# Assignment 2
# Date: November 23, 2014

##=================================================================
## **Description of function** There are two functions, 
## "makeCacheMatrix" and "cacheSolve." The goal is to cache (store) 
## solutions to matrices and return their value if called upon. 

##=================================================================
## **makeCacheMatrix**The function "makeCacheMatrix" creates an 
## empty matrix defined by "m", and is defined by matrix "x". 
##=================================================================
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }

##=================================================================
## **cacheSolve**. The function "cacheSolve" evaluates "m" to check 
## an inverse has already been calculated in "makeCacheMatrix". If
## the the inverse is stored and exists (evaluated by "!is.null"), 
## then the cached solution is returned. If it returns null, the
## function will evaluate the inverse of the new function. 
##=================================================================
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

##=================================================================
##-----------------------------END---------------------------------
##=================================================================
