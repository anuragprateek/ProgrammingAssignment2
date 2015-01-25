# The makeCacheMatrix funtion takes as input a matrix and creates a special matrix
# that is a list of 4 functions to:
#   1. Get the value of the matrix
#   2. Set the value of the matrix
#   3. Get the inverse of the matrix
#   4. Set the inverse of the matrix
# 
# If a normal matrix is passed to the cacheSolve function,
# the cacheSolve function internally calls the makeCacheMatrix function to convert
# the ordinary matrix to the "special" matrix and then finds the inverse, by 
# first looking at the cached value and if that does not exist by calling the 
# solve() function. If a "special" matrix is passed, the cacheSolve function
# does not call the makeCacheMatrix function internally.

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

# The function first checks if the passed parameter is a matrix or a special 
# matrix. If it is a matrix, the makeCacheMatrix function is called to convert it 
# into a special matrix, before calculations are continued.

cacheSolve <- function(y, ...) {
  if (is.matrix(y))
    x <- makeCacheMatrix(y)
  else
    x <- y
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
