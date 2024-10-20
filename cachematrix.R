## This pair of functions caches the inverse of a matrix to avoid redundant computations.
## `makeCacheMatrix` creates a special "matrix" object that can store the matrix and cache its inverse.
## `cacheSolve` computes the inverse of the matrix returned by `makeCacheMatrix`. If the inverse has 
## already been calculated (and the matrix has not changed), it retrieves the cached inverse.

## `makeCacheMatrix` creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize the cache for the inverse
  inv <- NULL
  
  # Function to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse cache when matrix is modified
  }
  
  # Function to get the value of the matrix
  get <- function() x
  
  # Function to set the value of the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the value of the cached inverse
  getInverse <- function() inv
  
  # Return a list containing all the above functions
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## `cacheSolve` computes the inverse of the special "matrix" created by `makeCacheMatrix`.
## If the inverse has already been calculated and the matrix has not changed, 
## `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # Retrieve the cached inverse
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, get the matrix and compute its inverse
  data <- x$get()
  inv <- solve(data, ...) 
  
  # Cache the inverse
  x$setInverse(inv)
  
  # Return the computed inverse
  inv
}
