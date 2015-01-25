## This scripts define functions makeChacheMatrix and chacheSolve refers to
## 2nd assigment of R Programming Coursera course.

## This function creates a special "matrix" object that can cache its inverse

## Receive a matrix returns a list with basics setters and getters, internals
## properties to store the original matrix value and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## stores inverse matrix of x
  invMatrix <- NULL
  
  ## define set value function
  set <- function(mat) {
    x <<- mat ## original value
    invMatrix <<- NULL ## reset inverse matrix if a new value was set
  }
  
  ## define get value fuction
  get <- function() x ## returns original value
  
  ## define set for the inverse value
  setInverse <- function (inverse) invMatrix <<- inverse
  
  ## define get for the inverse value
  getInverse <- function () { invMatrix }

  ## this function returns a List Object with this methods
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  ## checks the inverse matrix cache on X
  inv <- x$getInverse()
  if (!is.null(inv)){
    ## is cached, so returns the cache value
    message("using cached value")
    return(inv)
  }
  
  ## inv is null, so the cache is empty. Calcs the Inverse Matrix
  mat <- x$get()
  inv <- solve(mat)
  
  ## stores the inverse matrix in the cache
  x$setInverse(inv)
  
  ## returns the calculed inverse matrix
  inv
}
