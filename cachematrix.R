## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix takes an invertible square matrix (listmatrix) as input uses R's lexical scoping and list attributes
# to treat the matrix like a user defined object/class. The function declares a set of functions within its
# environment to act like methods for that object. Those methods are:
#-------------------------------------------------------------------
# -- set: uses <<- assignment to replace the existing listMatrix with newMatrix. 'set' then NULLS the current inverse
# of the object (but does not recalculate).
# -- get: returns the matrix currently stored in listMatrix
# -- setInverse: changes the current inverse attribute (listMatrixInverse) to the argument 'inverse'.
# -- getInverse: returns the current inverse attribute (listMatrixInverse)

makeCacheMatrix <- function(listMatrix = matrix()) {
    listMatrixInverse <- NULL
    
    set <- function(newMatrix) {
      listMatrix <<- newMatrix
      listMatrixInverse <<- NULL
    }
    
    get <- function() listMatrix
    
    setInverse <- function(inverse) listMatrixInverse <<- inverse
    
    getInverse <- function() listMatrixInverse
    
    #return list of methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## cacheSolve takes a 'list matrix' argument (a list created by makeCacheMatrix function that stores a matrix,
## the matrix's inverse, and it's 'methods') and returns the inverse of that 'list matrix'.

cacheSolve <- function(x, ...) {
  localInverse <- x$getInverse()
  if(!is.null(localInverse)) {
    message("getting cached data")
    
    # Stops the function and returns the current cached inverse if it exists.
    return(localInverse)
  }
  data <- x$get()
  localInverse <- solve(data, ...)
  x$setInverse(localInverse)
  
  ## Return a matrix that is the inverse of 'x'
  localInverse
}