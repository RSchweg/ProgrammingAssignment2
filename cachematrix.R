##This function creates a special "matrix" object that can cache its inverse.
##Use makevector.r as example to get started.

makeCacheMatrix <- function(x = matrix()){
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invx <<- inverse
  getinverse <- function() invx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve function should retrieve the inverse from the cache.
##Use cachemean.r as example to get started.

cacheSolve <- function(x,...){
  invx <- x$getinverse()
  if(!is.null(invx)) {
    message("getting cached inverse matrix data")
    return(invx)
  } 
  else {
    invx <-solve(x$get())
    x$setinverse(invx)
    return(invx)
  }
}  

