## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix accepts a matrix as an argument and returns a list containing four functions.
## The functions will store and retrieve the matrix and its inverse in memory.  Initially, the inverse will be NULL
## until the list is passed to the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve accepts a variable x and first tries to find the corresponding inverse matrix to the matrix stored in x
## x already contains an inverse matrix, and it is not NULL, it is returned.  If the matrix is not present in memory
## the function calculates the inverse matrix for the matrix returned by x$get(), and then stores it with x$setInverse()
## so that x now contains the inverse as well.
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
