## 2 functions are created. The first function makeCacheMatrix() 
## creates a list of functions to set / get the value of the 
## matrix and the inverse. The cacheSolve function computes the 
## inverse of the matrix returned by makeCacheMatrix

# 1st function - The makeCacheMatrix function creates a list of functions 
# to set/get the matrix/inverse of matri

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { #Set the value of the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x #get the value of the matrix
  setinv <- function(inverse) inv <<- inverse # set the value of the inverse
  getinv <- function() inv # get the value of the inverse
  #  Create a new object by returning a list, allowing "$" operator
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}

# 2nd function - The cacheSolve function computes the inverse 
# of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) { # checks to see whether the result is NULL
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) #solve function to get inverse of a matrix
  x$setinv(inv)
  inv
}


# test
m <- matrix(c(1,2,3,4),2,2)
m1 <- makeCacheMatrix(m)
m1$get() # retrieve the value of the matrix
m1$set(matrix(c(1,2,3,4),2,2)) # set the matrix value
m1$getinv() #retrieve the inverse of the matrix, which should be Null
cacheSolve(m1) #inverse matrix
m1$getinv() #retrieve the inverse of the matrix
