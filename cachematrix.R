## This function creates a matrix that can cache itself.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set<- function(a) {
    x<<- a
    m<<- NULL 
    ## sets value of matrix
  }
  get<- function() x ## gets value of matrix
  setinverse<- function(inv) m <<- inv ## sets value of inverse matrix
  getinverse<- function() m ## gets value of inverse matrix
  l<- list(set = set, get = get, setinverse = setinverse, getinverse=getinverse) ## sets values as list
  
}

## This function returns the inverse of the matrix - 1st checks for cached inverse matrix :: returns if present; if not present :: calculates & returns the inverse


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<- x$get()
  m<- solve(data, ...)
  x$setinverse(m)
  m
}
