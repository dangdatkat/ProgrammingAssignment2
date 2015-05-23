
# makeCacheMatrix - this function stores a matrix in cache and returns
# a list of functions and the memory in cache where the different 
# variables are store.


makeCacheMatrix <- function(x = matrix()) {
  # This function creates a special "matrix" enviornment that stores 
  # a cached matrix
  #
  # Args:
  #  x: matrix that can be inverted (from Function cacheSolve)
  #
  # Returns:
  #  A list of functions that perform tasks within the special "matrix"
  #  enviornment. 
  
  matrix.inverse <- NULL
  # creates a null inverse
  
  set <- function(y) {
    # resets the cached inverse by setting it to null
    x <<- y
    matrix.inverse <<- NULL
  }
  
  get <- function() x
  # the get function returns the matrix stored in solved.matrix
  
  setSolve <- function(solvedInverse) matrix.inverse <<- solvedInverse
  # the setSolve function actually stores the solvedInverse in 
  # matrix.inverse (cache)
  
  getSolve <- function() matrix.inverse
  # the getSolve function returns the stored matrix.inverse
  # the value will be NULL OR will return the cached solved value 
  
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
  # return the list of functions for the user
}



# cacheSolve - A complementary Function to makeCacheMatrix. The cacheSolve
# takes the a variable created from makeCacheMatrix and checks to see if
# the inverse is stalled in memory. If the inverse is saved, the function
# returns the inverse matrix.


cacheSolve <- function(x, ...) {
  # Looks for the matrix in the makeCacheMatrix Function. If it is NULL, the 
  # matrix will be solved and stored in matrix.inverse in Function makeCacheMatrix.
  # If matrix.inverse is not NULL, the inversed matrix is stored in cache.
  # This is the companion routine to makeCacheMatrix
  #
  # Args:
  #  x: a matrix that can be inverted
  #  ...: function from within the makeCacheMatrix function
  #
  # Returns:
  #  the value of the matrix inverse solved in the Function makeCacheMatrix
  
  matrix.inverse <- x$getSolve()
  # Return a matrix that is the inverse of 'x' from makeCacheMatrix
  
  if (!is.null(matrix.inverse)) {
    # checks to see if matrix.inverse is NULL. If the value is not Null,
    # no need to solve inverse 
    
    return(matrix.inverse)
    # returns the cached matrix inverse and exit the routine
  }
  
  # the matrix inverse was NULL, so it must be computed
  
  matrix.original <- x$get()
  # sets A to matrix prior to sending to makeCacheMatrix
  
  matrix.inverse <- solve(matrix.original, ...)
  # creates the matrix inverse
  
  x$setSolve(matrix.inverse)
  # saves the matric in cache
  
  matrix.inverse
  # returns the matrix inverse
}