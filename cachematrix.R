## Put comments here that give an overall description of what your
## functions do

## Description: the makeCacheMatrix function initializes a variable, inv, 
## within its environment that holds the solved inverse of the matrix. It also
## returns a list of four functions that can set and get the original matrix,
## and set and get the calculated inverse of the matrix. 

## Arguments: Any nxn matrix
## Returns: a list of functions

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL #Initialize inv variable within makeCacheMatrix environment
  
  #Define the first function, to set the matrix
  #y is defined only within the set function's env within makeCacheMatrix
  
  set <- function(y){
    x <<- y #When set() is called within the makeCacheMatrix env, will set new value for x
    inv <<- NULL #if the matrix gets reset, need to reset inv to NULL
  }
  
  get <- function(){
    x #get() will just return the current value of x
  }
  
  setinv <- function(inverse){
    
    inv <<- inverse #Set the inv variable, which exists in mCM env, to supplied value
    
  }
  
  getinv <- function(){
    inv #getinv() will just return the current value of inv; will return NULL if not setinv()
  }
  
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)){ #execute branch if inv is not NULL
    inv
  }
  
  else{ #If inv is NULL, first setinv() and then return inv
    
    data <- x$get() #supply matrix to data variable within cacheSolve
    inv <- solve(data) #supply value of inverse to inv
    
    x$setinv(inv) #supply value of inverse to setinv() for x
    inv #return value of inv after it is set
  }
}