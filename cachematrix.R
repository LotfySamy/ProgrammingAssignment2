# makeCacheMatrix is a function that creates a square matrix from a vector. 
# Inputs to this function are a vector and an integer number (n) which is the order of matrix.
# The function creates a matrix with dimensions (n*n).
# You will be able to set the matrix and set its inverse. 
# You also able to get the matrix and its inverse.

makeCacheMatrix <- function(x = numeric(),n=2){
  m <- NULL
  dim(x) <- c(n,n)
  x
  set <- function(y = numeric()) {
    dim(y) <- c(n,n)
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(m) m <<- solve(x)
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# cacheSolve is a function that checks if the inverse matrix is cached in memory. 
# If it is cached, it will give you a message that the inverse matrix is cached.
# If it is not cached, it will calculate the inverse of the matrix and cache it.

cacheSolve <- function(x=numeric()) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}