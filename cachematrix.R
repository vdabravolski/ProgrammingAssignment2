## makeCacheMatrix function returns the list of functions "set", "get", "setinverse" and "getinverse".
## These functions manipulate with variables "x" (original matrix) and "m" (inverted matrix).
## Variable "m" is cache variable and either contains NULL (cache is empty) or inverted matrix (cache exists and is valid).
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) { # function to set new value for 
    x <<- y
    m <<- NULL #nulling the matrix parameters since we want to recalculate it
  }
  
  get <- function() x
  
  getinverse <- function() m
  
  setinverse <- function(z){
    m <<- z
  }
  
  list(set=set, get=get, getinverse=getinverse, setinverse=setinverse)
}


## cacheSolve returns the inverted matrix. 
## If cache is not empty than it simpy returns cache (variable "m"). 
## If cache is empty than it calculates the inverted martix and refreshes the cache (variable "m")
## Input parameters: function makeCacheMatrix
## Output: inverted matrix (variable "m")
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)){ # check if cache ("m" variable) is empty or not.
    message("getting cached data")
    return(m) # return current cache
  }
  
  data <-x$get()
  m <- solve(data) # since cahce is empty, recalculate the cache.
  x$setinverse(m)
  m
}


