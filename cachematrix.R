## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# x is an inversible matrix
# Inv is defined as the inverse of matrix x
# This function has four fucntions
# 1. function set(): set the value to matrix x
# 2. function get(): get the value of matrix x
# 3. function setinv(): set the value of the inverse matrix of x
# 4. function getinv(): get the value of the inverse matrix of x

makeCacheMatrix <- function(x = matrix()) {
  Inv<-NULL
  set<-function(y){
    x<<-y
    Inv<<-NULL
  }
  get<-function() x
  setinv <- function(INVS) Inv<<-INVS
  getinv <- function() Inv
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# This function get the inverse of matrix x
# It first cache the inverse of matrix x if it was calucated or set by function x$setinv()
# If it cannot find the value, it will calculate the inverse of matrix x by calling solve(）

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Inv <- x$getinv()
  if(!is.null(Inv)){
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data)
  x$setinv(Inv)
  Inv
}
