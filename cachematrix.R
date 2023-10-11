##The code defines two functions, CacheMatrix and cacheSolve, 
## that together provide a way to efficiently calculate and cache 
## the inverse of a matrix in R. 

## The makeCacheMatrix function creates a custom "matrix" object 
## with the ability to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set = set, 
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## The cacheSolve function is designed to calculate the inverse of a matrix.
## It takes one argument, x, which is expected to be a custom "matrix" object 
## created using the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  inv
}
