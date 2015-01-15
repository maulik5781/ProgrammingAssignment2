## Below are two functions  to calculate an inverse of a given matrix
## Example Execution : 
##    source("cachematrix.R")
##    x<- matrix(rnorm(200), 20,10)
##    y<-makeCacheMatrix(x)
##    cacheSolve(y)
##    compare the result with solve(x)

## 1. makeCacheMatrics, which takes in a matrix object and
## returns a matrix object that can cache the inverse of a given matrix
## 2. cacheSolve, which takes in a cached matrix object and returns an inverse of it while
## internally using the cached inverse matrix 

## makeCacheMatrix function has the inverse object, set, get, setinvers and getinverse methods
## this function creates a matrix that can cahe the inverse of the object internally

makeCacheMatrix<-function(x= matrix()){
  # This is the inverse object
  inverse <- NULL
  # This is the set function for original matrix
  set <- function(y){
    x<<-y
    inverse <<- NULL
  }
  # This is the get function to get the value of matrix from environment
  get<- function() x

  # these are inverse functions set/get to set or get inverse of the matrix
  setinverse<- function(inverse_1) inverse<<-inverse_1
  getinverse <- function() inverse
  
 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve is a function which receives the matrix object and checks if inverse already exists
## if inverse exists then it returns it, otherwise it solves for inverse

cacheSolve<- function(x=matrix(),...){
  # This gets the inverse of the matrix if exists
  inverse<- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # we did not have stored value of inverse so we will calculate it ourselves
  data <- x$get()  #get actual matrix
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  # return inverse of the matrix
  inverse
  
}
