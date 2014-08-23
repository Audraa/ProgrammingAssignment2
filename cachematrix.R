#The first function, makeCacheMatrix creates  special "matrix" object which is a
#list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

## The makeCacheMatrix creates a list object. Within the list object we have 
## the functions get,set, setInverse and getinverse. The get inverse retuns the
## matrix in the list object. The set function resets the value of i which is
## the cached inverse matrix. The getinverse returns the cached index i The Set
## inverse calculates the invers of the matrax and stores it as the cached
## index.


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


## cachesolve returns the inverse of a matrix within a list object. x passed is
## of class list. This can be verified using class(x) where x is the object that
## was created using make Matrix. When the call is made firs check that it is a
## square matrix. we do this by checking nrow and ncol for the matrix This is not 
## required for the assignment as assume that the matrix supplied is always invertible
## that is, it is a square matrix .

cacheSolve <- function(x, ...) {
  r<- nrow(x$get())
  c<-ncol(x$get())
  
  if (!r==c){message("Please provide a square matrix.The question asks to solve the inverse of a square matrix")
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
# SAMPLE 1 - Non square matrix.
## Sample code to test 
## mat<-matrix(1:6,3,2)
## iMatrix<-makeCacheMatrix(mat)
##  iMatrix
## cacheSolve(iMatrix)
## Result:  Please provide a square matrix.The question asks to solve the inverse of a square matrix
## NULL

# SAMPLE 2  Square matrix
##  >mat<-matrix(1:4,2,2)
## >mat
#      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > iMatrix<-makeCacheMatrix(mat)
## > cacheSolve(iMatrix)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
