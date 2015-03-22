# Both functions below cache the inverse of a matrix.

#The function below takes a matrix as argument in order to store it
makeCacheMatrix <- function(x = matrix()) {
  #The variable below will store the Inverse of the Matrix.
  inverse <- NULL
  
  #This function gets a new matrix
  set <-function(newMatrix) {
    x <<- newMatrix
    inverse <<- NULL
  }
  
  #This function returns the matrix
  get <- function() x
  #This function gets a new inverse
  setInverse <- function(newInverse) inverse <<- newInverse
  #This functions return the Inverse of The Matrix
  getInverse <- function() inverse
  #In the end, a list that contains all the function will be returned
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}
#This function calculates a Inverse of a matrix if it was not done yet.
cacheSolve <- function(x, ...) {
  
  inverse <- x$getInverse() #It gets the current Matrix Inverse
  if(!is.null(inverse)){ #If it is not null then returns the inverse
    message("getting cached inverse matrix")
    return(inverse)
  }
  #If it is null then calculates the Matrix Inverse and returns it
  matrix <- x$get()
  temp <- solve(matrix)
  x$setInverse(temp)
  temp
}
