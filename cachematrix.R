makeCacheMatrix <- function(x = matrix()){
  # 'x' is assumed to be a square and invertible matrix.
  # This function takes a given matrix and assigns the inverse 
  # to a cache matrix in order to make later computation easier
  
  
  CacheMatrix <- NULL
  set <- function(y){
      
      # Use " <<- " to set value of x in a different environment
      x <<- y
      CacheMatrix <<- NULL
      
  }
  
  get <- function()x
  
  # inverts matrix and stores it in "CacheMatrix"
  setmatrix <- function(inverse)CacheMatrix <<- inverse
  
  getinverse <- function()CacheMatrix
  
  list(set = set, get = get, setmatrix = setmatrix, getinverse = getinverse)
  
}

cacheSolve <- function(x, ...) {
  # cacheSolve solves for the inverse of the makeCacheMatrix function.
  
  CacheMatrix <- x$getinverse()
  
  # Checks if inverse has already been calculated.
  # If so, it skips the computation
  if(!is.null(CacheMatrix)) {
    
    message("getting cached data")
    return(CacheMatrix)
    
  }
  
  # Calculates inverse it not already done.
  mydata <- x$get()
  CacheMatrix <- solve(mydata, ...)
  x$setmatrix(CacheMatrix)
  
  return(CacheMatrix)
  
}
