## Put comments here that give an overall description of what your
## functions do

# Function to create the matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL  # Initialize the cached inverse as NULL
  
  # Setter for the matrix
  set <- function(y) {
    x <<- y        # Set the matrix
    inv <<- NULL   # Reset the cached inverse
  }
  
  # Getter for the matrix
  get <- function() {
    x
  }
  
  # Setter for the cached inverse
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Getter for the cached inverse
  getInverse <- function() {
    inv
  }
  
  # Return a list of methods to interact with the matrix and its inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Function to compute the inverse of the matrix, with caching

cacheSolve <- function(x, ...) {
# Check if the inverse is already cached
  inv <- x$getInverse()
  
  # If cached inverse exists, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # If not cached, compute the inverse using solve() and cache it
  data <- x$get()  # Get the matrix
  inv <- solve(data, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the computed inverse
  
  inv  # Return the inverse
}
