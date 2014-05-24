## Matrix inversion is usually a costly computation and their may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. Bellow are
## two functions that are used to create a special object that stores a matrix and
## cache's its inverse.

## The first function, makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse
## 4. get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}



## The following function calculates the inverse of the matrix created with the above
## function. It first checks if the inverse has already been calculated. If so, it gets
## the result and skips the computation. Otherwise, it calculates the inverse, sets the
## value in the cache via setinverse function.

## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
 
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data.")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

## Example:
## x <- matrix(rnorm(4), nrow = 2)  //Create a matrix 'x'
## cm <- makeCacheMatrix(x)         //Special matrix
## cacheSolve(cm)                   //Inverse of the matrix
## cacheSolve(cm)                   //Return cached inverse
