## Put comments here that give an overall description of what your
## functions do
## The functions compute and cache the inverse of a square matrix

## Write a short comment describing this function
## makeCacheMatrix creates a list containing functions, 
## which are needed to cache the inverse of a square matrix.

makeCacheMatrix <- function(x = matrix()) {

inv <- NULL
  set <- function(y){
    x <-- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function () inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## cacheSolve is the function which checks if the inverse has already been computed,
## if so it recieves the inverse directly from cache,
## otherwise the inverse will be computed und cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
