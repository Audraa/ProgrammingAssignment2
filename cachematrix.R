## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cachesolve returns the inverse of a matrix within a list object. x passed is of class list. This can be verified using class(x) where x is the object that was created using make Matrix.
## When the call is made firs check that it is a square matrix. we do this by checking nrow and ncol for

cacheSolve <- function(x, ...) {
  r<- nrow(x$get())
  c<-ncol(x$get())
  
  if (!r==c){message("We are only calculating the inverse of a square matrix")
             return(NULL)}
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

#########################################

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

