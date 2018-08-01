## Put comments here that give an overall description of what your functions do

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                   # Initialize the inverse property.
  
  set <- function(y) {                        # Set the value of vector
    x <<- y                                   # update the old matrix to the new one
    m <<- NULL                                # reset the inverse of the matrix
  }
  get <- function() x                         # Method to get the actual matrix and return the matrix
  setinverse <- function(inverse) m <<- inverse   # Set the inverse of the matrix
  getinverse <- function() m                  # Get the inverse of the matrix
  list(set = set, get = get,                  # Return a list of the available functions
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
                                          
  m <- x$getinverse()                     # Return a matrix that is the inverse of 'x'
  if(!is.null(m)) {                       # check if this inverse of the matrix has been calculated
    message("getting cached data")        # if so, print "getting cached data" and
    return(m)                             # returns the inverse of the matrix.
  }
  #If the inverse has not been calculated:
  data <- x$get()                         # Get the matrix       
  m <- solve(data, ...)                   # Calculate the inverse of the matrix using solve
  x$setinverse(m)                         # Updating the variable, set the inverse of the matrix
  m                                       # Return the matrix
}


