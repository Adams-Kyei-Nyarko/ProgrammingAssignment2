#the first function which is makecahcematrix helps to define the argument which is the matrix
#The second function in the next environment which is the set
#helps to set the values for the matrix; anytime varibale x is changed it affects 
#y in all the environments
#The get function helps to return the inputs of x
#The get inverse function also helps to get the inputs for the inverse matrix
#which returns null until the cache solve function is used to calculate the 
#inverse of the assigned matrix values

makeCacheMatrix <- function(x = matrix()){#the matrix here is invertible
  i <- NULL #Note that the single assignment operator only assigns 
  #values to the current environment unlike the superassignment operator
  set <- function(y){#this function then sets the values for the matrix
    x <<- y #superassignment operator was used here.
    #It helps to assign values in the enclosing environment;
    #that is it tries to identify the variable by looking through till the global 
    #environment meaning it searches through all the structure of environments
    i <<- NULL # The null as used to represent a list of 0 length
  }
  get <- function() x
  setinverse <- function(inverse){i <<- inverse}
  getinverse <- function()i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cachesolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting solved cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  return(i)
}





