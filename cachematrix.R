## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Variable to store the cached inverse matrix
  
  # Method to set the matrix and reset the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Method to get the matrix
  get <- function() x
  
  # Method to set the inverse matrix (cache it)
  setInverse <- function(inverse) inv <<- inverse
  
  # Method to get the cached inverse matrix
  getInverse <- function() inv
  
  # Return a list of all methods
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Try to get the cached inverse
  
  if (!is.null(inv)) {   # If inverse is already cached
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()        # Get the matrix from the object
  inv <- solve(data, ...)  # Compute the inverse using solve()
  x$setInverse(inv)      # Cache the computed inverse
  inv                    # Return the inverse
}



## testing the functions
m <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))
# First call calculates the inverse
cacheSolve(m)
# Second call retrieves the cached inverse
cacheSolve(m)
