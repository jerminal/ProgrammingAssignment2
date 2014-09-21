## Function "makeCacheMatrix" creates vector  which containing functions to set and get the matrix and to set 
## the inverse and get the inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function "cacheSolve" calculates the inverse of the vector, created with the "makeCacheMatrix" function. 

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
    # if the inverse in not null, we can return the cached version
    message("getting cached data")
  } 
  else {
    # otherwise, we will calculate the inverse and save it in the cache
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
  }
  
  inverse
}