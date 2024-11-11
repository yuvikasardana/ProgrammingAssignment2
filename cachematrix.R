## Put comments here that give an overall description of what your
## functions do
##The function makeCacheMatrix takes a matrix as its argument and returns a list with 
##functions to set the matrix , get the matrix set its inverse and get its inverse
##The second function cacheSolve gets the inverse of the matrix through the function 
##getInverse if it is already cached otehrwise it will calculate the inverse set it and
###then return the same


##i this function is for returning the list with the functions:
##set : to set a new matrix and the set the value of inverse to NULL
##get : to get the value of the matrix
##setInverse: to set the value of its inverse
##getInverse : to get the value of its inverse if it is already cached
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL  
  set <- function(y) {
    x <<- y   
    i<<- NULL  
  }
  get <- function() x  
  setInverse <- function(inverse) i <<- inverse  
  getInverse <- function() inv  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 

}


## cacheSolve will first check if the inverse of the matrix is already cached with the 
##help of the function getInverse if yes tehn it will return the inverse and if not 
##then it will calculate the inverse and set its value with the help of function
##setInverse and then again returns the inverse

cacheSolve <- function(x, ...) {
  i <- x$getInverse()  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()  
  i <- solve(mat, ...)
  x$setInverse(i) 
  i
}



