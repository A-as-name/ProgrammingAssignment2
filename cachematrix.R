## Function makeCacheMatrix and cacheSolve saved in same file

## makeCacheMatrix creates a special "Matrix", which does
##(1) set the value of the matrix
##(2) get the value of the matrix
##(3) set the value of the inverse
##(4) get the value of the inverse
##(5) return a list of 4 function

makeCacheMatrix <- function(x = matrix()) {
  inverse_m <- NULL
  
  ## (1)
  set <- function(y){
    x <<- y
    inverse_m <<- NULL
  }
  
  ## (2)
  get <- function() x
  
  ## (3)
  setinverse <- function(inverse) inverse_m <<- inverse
  
  ## (4)
  getinverse <- function() inverse_m
  
  ## (5)
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Function CacheSolve is a function that computes the inverse of the matrix created
## If the inverse exists, then it will retrieve that while displaying to user that 
## it is retrieving that value from  cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_m <- x$getinverse()
  
  if(!is.null(inverse_m)) {
    message("getting cached data")
    return(inverse_m)
  }
  
  data <- x$get()
  inverse_m <- solve(data, ...)
  x$setinverse(inverse_m)
  inverse_m
  
}
