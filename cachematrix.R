## Put comments here that give an overall description of what your
## functions do

## The functions in this R script are use to create a cacheable Matrix object and 
## cache its inverse so that future computation are provided faster.


## Write a short comment describing this function

## makeCacheMatrix is a function that tranforms an ordinary matrix x 
## into a cacheable matrix object  
## sample usage:
##   a <- matrix(c(4,2,7,6),nrow=2)     ### creates an ordinary 2x2 matrix, a
##   b <- makeCacheMatrix(a)            ### creates a cacheable matrix, b, based from matrix a      

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInverse <- function(solve) m <<- solve
     getInverse <- function() m
     list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

## cacheSolve is a function that computes the inverse of the matrix, x, 
## if it was not computed previously. Otherwise, it returns the result
## of the previous inverse computation, which was stored in the cacheable 
## matrix object
## Usage:
##   cacheSolve(b)  ### where b is a cacheable Matrix object

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getInverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setInverse(m)
     m
}
