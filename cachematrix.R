## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


##returns a vector, containing functions to get() and set() the matrix and getinv() for getting the inverse matrix
##and setinv() for setting it
makeCacheMatrix <- function(x = matrix()) {
  ##inner property m for storing inverse matrix
  m <- NULL
  
  ##definition of set function
  set <- function(y) {
    ##giving new value to f-n variable x by assignment to parent environment 
    x <<- y
    
    ##if the matrix has changed, we are removing the previously caslculated inverse
    m <<- NULL		
  }
  
  
  get <- function() {
    ##returning x - which stores the original or altered later value of matrix -- from def. env.
    x
  }
  
  setinv <- function(inv) {
    ##setting the nev value of inverse by assignment to parent environment 
    m <<- inv
  }
  
  getinv <- function() {
    ##returning the inverse matrix variable (from defining environment, as it has not been defined within the f-n
    m
  }
  
  ##returning the list of named functions as a result of encompassing function
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##this function takes the matrix function vector and checks whether the inverse for matrix has already been calculated
##if not, calculates and stores it; if yes, returns the stored value

cacheSolve <- function(x, ...) {
  ##we call the function getinv to acquire the inverse matrix or non-defined value
  m <- x$getinv()
  
  if(!is.null(m)) 
  {
    ##it is not NULL, so it was calculated. we return it
    ##print("cached value")
    return(m)
  }
  
  ##it was null, so we need to calculate it

  ##we get the original matrix by calling get() function defined in "vector"
  data <- x$get()
  
  ##we calculate the inverse by means of ginv()
  m <- solve(data, ...)
  
  ##we set the inverse matrix value in the vector by calling setinv
  x$setinv(m)
  
  ##returning the inverse matrix
  ##print("calculated")
  m
}
