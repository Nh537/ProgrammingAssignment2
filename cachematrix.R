## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix consists if set, get, setinverse, getinverse
#library(MASS) is used to calculate inverse
library(MASS)

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)inv<<-inverse
  getinverse<-function(){
    inver<-ginv(x)
    inver%*%x
    
  }
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
  }
    




## Write a short comment describing this function
#This is used to get cache data
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cache data") #caches the data
    return(inv)  #this returns the inverse
  }
  data<-x$get()
  inv<-solve(data....)
  x$setinverse(inv)
  inv
}
  

