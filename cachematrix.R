# Function to make cache matrix with default 0,0 dim matrix 
makeCacheMatrix <- function(mat = matrix(nrow=0, ncol=0)){
  inv <- NULL   # inverse
  
  # Function to set and cache matrix and its inverse
  set <- function(y){  
    mat <<- y
    inv <<- NULL
  }
  
  get <- function() mat  # Function to return matrix
  
  setinv <- function(v) inv <<- v  # "<<-" is used here to assign a value to an object in a different environment
                                   # Function to cache and store value of calculated inverse
                                   # This "cache" value will be used once calculated (without having
                                   # to re-calculate this value every time)
  
  getinv <- function() inv         # Function to return inverse   
  
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

# Function cacheSolve calculates inverse of the special matrix returned by makeCacheMatrix, 
# accesses cached value if inverse is already calculated
cacheSolve <- function(m, ...){
  inv <- m$getinv()
  
  # If inv already exists, then get its value from cache
  if(!is.null(inv)){ 
    message("getting cached data")
    return(inv) 
  }
  
  mat <- m$get()
  
  inv <- solve(mat)
  
  m$setinv(inv)
  
  return(inv)
}

# Sample run
# x <- matrix(c(1,2,3,4), nrow=2, ncol=2)
# y <- makeCacheMatrix(x)
# cacheSolve(y)
# cacheSolve(y)
# getting cached data

