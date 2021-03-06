## Caching (store objects in in memory) the Inverse of a Matrix:
# 
# Matrix inversion is usually a costly operation and it is advantageous to cache
# it in the memory rather than compute it repeatedly. Below is a pair of
# functions that cache the inverse of a matrix.

# makeCacheMatrix() creates an R object that stores (caches) the matrix and its
# inverse. The return object is of list type, so its elements can be accessed
# using $ notation.

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize objects x (default to an empty matrix) and i set to null
  i <- NULL
  ## set function:
  ## 1. Assign input argument y to x in parent environment (by using <<-)
  ## 2. Assign NULL to i, that clear any value of i cached from prior run
  ## run of cacheSolve().
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## getter for matrix x
  get <- function() x
  ## setter for inverse i (i is defined in parent environment with <<-)
  setinv <- function(inv) i <<- inv
  ## getter for inverse i
  getinv <- function() i
  # return list of setter and getter functions (note: there are no parenthesis())
  return(list(set=set, get=get, setinv=setinv, getinv=getinv))
}

# cacheSolve() takes argument returned by makeCacheMatrix() to retrieve the
# value of inverse.  If the inverse has already been calculated (and the matrix
# has not changed), then the cachesolve should retrieve the inverse value from
# the cache. It computes the inverse of the input matrix returned by function
# makeCacheMatrix (if it is of type makeCacheMatrix) if it is not calculated
# before. 


cacheSolve <- function(mvec, ...) {
  ## get the inverse using getter function for inverse getinv() 
  ## if it is calculated it will return value otherwise i will be NULL
  i <- mvec$getinv()
  
  ## if i is already calculated return i with the message
  ## and get out of function and ignore remaining steps
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## if i is NULL, then get the data using getter function get()
  data <- mvec$get()
  ## calculate inverse with solve()
  i <- solve(data, ...)
  ## set the value of i using setter function setinv()
  mvec$setinv(i)
  ## return value of i
  return(i)
}

###### Checking if functions work properly
# x <- matrix(c(1, 2, 3, 5), nrow = 2, ncol = 2)
# a<- makeCacheMatrix(x)
# cacheSolve(a)
## This returns following matrix (inverse of x)
# [,1] [,2]
# [1,]   -5    3
# [2,]    2   -1

# cacheSolve(a)
## Running cacheSolve(a) again uses cached data, so don't need to calculate
## inverse again
# getting cached data
# [,1] [,2]
# [1,]   -5    3
# [2,]    2   -1


