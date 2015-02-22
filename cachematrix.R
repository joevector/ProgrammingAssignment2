## These two functions allow one to work with a special type of
## object that can store information regarding a matrix and its
## inverse. Given that calculating an inverse can be computationa-
## lly expensive, doing so via these functions will store the
## result so that it can be retrieved in the future without having
## to redo the calculation process.

## This function will generate an object (list type) that contains
## functions which allow setting and reading information regarding
## a matrix: its contents per se and its inverse, which once calc-
## ulated is cached.

makeCacheMatrix <- function(x = matrix()) {
  
  #The object has just been created, which means the inverse
  #hasn't yet been calculated so we set its value to NULL.
  i <- NULL
  
  #This object allows the user to change the data of the matrix
  #it stores. Doing so means we need to recalculate the inverse,
  #so we set it to NULL in order to wipe any previous data.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #This function permits retrieval of stored matrix data.
  get <- function() x
  
  #The value of the inverse is calculated elsewhere and then
  #stored in the object via this function.
  setInverse <- function(inverse) i <<- inverse
  
  #When the inverse data is already present in the object,
  #this function allows retrieval of that information.
  getInverse <- function() i
  
  #This will output the desired object, a list containing the
  #functions which allow storage and retrieval of the relevant
  #data.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function will return the inverse of the matrix stored in
## the object passed as 'x'. First, it checks the object for pre-
## calculated data which it returns if present. Lacking any such
## cached information it will calculate the matrix's inverse and
## use the object's cache function to store the result, then ret-
## urning the desired inverse.
##
## (any extra arguments for the solve function are allowed via
##  use of the "..." argument in the function)

cacheSolve <- function(x, ...) {
  
  #We retrieve the data for the inverse stored in the object.
  i <- x$getInverse()
  
  #If the data is already present, we simply retrieve and return
  #that information.
  if(!is.null(i)) {
    message("Retrieving cached inverse")
    return(i)
  }
  
  #If it isn't, we retrieve the matrix information...
  data<-x$get()
  
  #... we calculate the inverse, passing any additional arguments...
  i<-solve(data, ...)
  
  #... and store it in the object so calculating it again isn't
  #necessary.
  x$setInverse(i)
  
  #Finally, we return the value of the inverse which we just calculated!
  i
}
