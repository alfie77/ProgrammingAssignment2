## Put comments here that give an overall description of what your
## functions do

## **NOBODY** EXPECTS THE SPANISH INQUISITTION!

## Please note that x_1 identifies the inverse of matrix x (as in x to the minus 1)

makeCacheMatrix <- function(x = matrix()) {
  x_1 <- NULL
  set <- function(y) {
    x <<- y
    x_1 <<- NULL
  }
  get <- function() x
  setInv <- function(solve) x_1 <<- solve
  getInv <- function() x_1
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## this function checks for existence of the calculated inverse:
##
##  * If it is *not* present:
##        ** it runs solve to create such inverse
##        ** stores the new inverse in x_1 
##        ** returns the inverse just calculated. 
##
##  * If it *is* present:
##        ** it will say so
##        ** it returns the inverse as it is stored, without re-calculating anything.

cacheSolve <- function(x, ...) {

  x_1 <- x$getInv()
  if(!is.null(x_1)) {
    message("getting cached data")
    return(x_1)
  }
  data <- x$get()
  x_1 <- solve(data, ...)
  x$setInv(x_1)
  x_1
}
