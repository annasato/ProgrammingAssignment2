## R Programming - Week 3 - Assignment 2

## Put comments here that give an overall description of what your
## functions do

## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


## Check

A <- matrix( c(5,1,0,
               3,-1,2,
               4,0,-1), nrow=3, byrow=T)
det(A) #inverse exists

AI <- solve(A)
AI

AI %*% A #results in identity matrix

makeCacheMatrix(x=A)
cacheSolve(makeCacheMatrix(A))

