## Put comments here that give an overall description of what your
## functions do:- 
## The first function, makeCacheMatrix creates a special vector, which is
## nothing but a list containing a function to set the value of the matrix,
## get the value of the matrix, set the value of the inverse and get the
## value of the inverse. Here the the <<- operator is used to 
## assign a value to an object in an environment that is different from 
## the current environment. 
## The cacheSolve function calculates the inverse of the makeCacheMatrix functi
## -on created above. However, it first checks to see if the inverse has already been
##calculated. If so, it gets the inverse from the cache and skips the
##computation. Otherwise, it calculates the inverse of the matrix and sets the
## value of the mean in the cache via the setInverse function.


## Write a short comment describing this function:-
##The makeCacheMatrix function is a list of functions to set and get the
##value of the matrix and that of the inverse of the matrix and also cache it

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y){
    x<<-y
    inv<<- NULL
  }
  get<- function() {x}
  setInverse<- function(inverse) {inv<<- inverse}
  getInverse<- function() {inv}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function:-
##The cacheSolve function calculates the inverse of the makeCacheMatrix function
##created above. If the inverse is found to be already calculated, it skips the 
##computation and gets the inverse from the cache. Otherwise, it calculates the 
## inverse of the matrix and sets the value of the mean in the cache via the 
##setInverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  }
