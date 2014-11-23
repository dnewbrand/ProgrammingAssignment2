## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize cache 
  m <- NULL
  
  ## Set the matrix    
  set <- function(y) {
    x <<- y 
    m <<- NULL 
  }
  
  ## Return the matrix x
  get <- function() x 
  
  ## Set the inverse of the matrix
  setinverse <- function(inverse) m <<- inverse 
  
  ## Get the inverse of the matrix
  getinverse <- function() m 
  
  ## Return a list    
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of x 
  m <- x$getinverse()
  
  ## Return matrix if cached    
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If not cached, calculate the inverse and return the matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

