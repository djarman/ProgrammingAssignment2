## Devon Jarman
## Coursera: R Programming
## Week 3
## Solution to Programming Assignment 2 

## 1.  `makeCacheMatrix`: This function creates a special 'cacheMatrix' object
## that can cache its inverse.

## 2.  `cacheSolve`: This function computes the inverse of the special
## 'cacheMatrix' returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` will retrieve the inverse from the cache.

## 

## This function creates a special 'cacheMatrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  ## 'x' is a square matrix.
  
  ## return 'cacheMatrix' object.
  
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


## This function computes the inverse of the special 'cacheMatrix' returned by `makeCacheMatrix`.
## If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` will retrieve the inverse from the cache. Computing the inverse of a square matrix 
## is done with the `solve` function (assumes that the matrix supplied is always invertible).

cacheSolve <- function(x, ...) {

  ## 'x' is a 'cacheMatrix' object.
  
  ## return inverse of 'cacheMatrix'.
  
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
