## Coursera Week 3, Assignment 2 by Claudia Durkin###


# MakeCache Matrix does the following:
# a) It sets the value of the matrix 
# b) Gets the value of the matrix
# c) Sets the value of the inverse of the matrix 
# d) Gets the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getinverse<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getinverse=getinverse)
}


## CacheSolve returns the inverse of the matrix, first checking to see if the inverse has been computed.  
#If it has been computed, gets the result otherwise, it computes the inverse ans sets the value.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
