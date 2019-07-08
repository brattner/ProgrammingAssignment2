# makeCacheMatrix() is a function that builds a set of functions with the goal of
# storing a matrix and its inverse.

# makeCacheMatrix() contains the following four functions:

# 1. set()      sets the value of a matrix
# 2. get()      gets the value of a matrix
# 3. setInverse()   sets the value of the inverse of a matrix 
# 4. getInverse()   gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = numeric()) {
  
  # create inverse.matrix to hold the inverse of the matrix. 
  # at the beginning it will be NULL
  
  inverse.matrix <- NULL
  
  # store a matrix in element x
  
  set <- function(y) {
    x <<- y
    
    # since the matrix is assigned a new value, flush the value of 
    # inverse.matrix to NULL
    
    inverse.matrix <<- NULL
  }
  
  # return the stored matrix
  
  get <- function() {
    x
  }
  
  # calculate (get) the value of the inverse of the given matrix
  
  setInverse <- function(solve) {
    inverse.matrix <<- solve
  }
  
  # set the stored value for the inverse of the given matrix (inverse.matrix)
  
  getInverse <- function() {
   inverse.matrix
  }
  
  # return a list (each named element of the list is a function)
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# The function cacheSolve() calculates the inverse of a "special" matrix created with 
# makeCacheMatrix()

cacheSolve <- function(y, ...) {
  
  # get the value stored in inverse.matrix
  
  inverse.matrix <- y$getInverse()
  
  # if a stored value exists, return it
  
  if(!is.null(inverse.matrix)) {
    message("getting cached data")
    return(inverse.matrix)
  }
  # otherwise, get the matrix, caclulate the inverse and store it in
  # inverse.matrix
  
  data <- y$get()
  inverse.matrix <- solve(data)
  y$setInverse(inverse.matrix)
  
  # return the inverse
  inverse.matrix
}
