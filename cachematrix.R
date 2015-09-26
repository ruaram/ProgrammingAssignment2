## Matrix inversion is usually a costly computation
## There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
## The two functions below accomplish the feat


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL
  
  # set a new matrix
  set <- function(y) {
    
    x <<- y ## set provided matrix as the new cached matrix
    
    m_inverse <<- NULL ## reset cached inverse to NULL
  }
  
  # returns the matrix data i.e. the original matrix
  get <- function() x
  
  # sets the inverse as provided in the argument
  setinverse <- function(inverse) m_inverse <<- inverse
  
  # returns the matrix inverse
  getinverse <- function() m_inverse
  
  
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Attempt to get a new the cached inverse
  m_inverse <- x$getinverse()
  
  ## return cached inverse if available
  if(!is.null(m_inverse)) {
    message("getting cached inverse")
    return(m_inverse)
    
  }
  
  ## otherwise calculate the inverse of the matrix
  data <- x$get()
  m_inverse <- solve(data) ## solve(x) retruns the inverse of the matrix assuming x is an invertible matrix
  
  ## set the calculated inverse as the cached inverse
  x$setinverse(m_inverse)
  
  ## return the inverse
  m_inverse
}
