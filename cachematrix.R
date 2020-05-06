## Put comments here that give an overall description of what your
## functions do

## wrapper function that stores get, set, getinverse, setinverse functions
## for matrix cache handling

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
        
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## function to get inverse, looks up the inverse in the cache first,
## if not found calculates the inverse and stores in the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
        
  if(!is.null(m)) {
      return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
        
  m
}
