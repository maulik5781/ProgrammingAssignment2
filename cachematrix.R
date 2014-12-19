## Below are two functions  to calculate an inverse of a given matrix
## 1. makeCacheMatrics, which takes in a matrix object and
## returns a matrix object that can cache the inverse of a given matrix
## 2. cacheSolve, which takes in a cached matrix object and returns an inverse of it while
## internally using the cached inverse matrix 

## makeCacheMatrix function has the inverse object, set, get, setinvers and getinverse methods
## this function creates a matrix that can cahe the inverse of the object internally

makeCacheMatrix<-function(x= matrix()){
  inverse <- NULL
  set <- function(y){
    x<<-y
    
  }
  
  get<- function() x
  
  setinverse<- function(inverse_1) inverse<<-inverse_1
  getinverse <- function() inverse
  
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}




## cacheSolve is a function which receives the matrix object and checks if inverse already exists
## if inverse exists then it returns it, otherwise it solves for inverse

cacheSolve<- function(x=matrix(),...){
  inverse<- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()  #get actual matrix
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  
}
