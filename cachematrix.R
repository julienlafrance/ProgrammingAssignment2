## JLF - March 2016 
## Store in cache the Inverse of a matrix tto make available later on


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ## Define different methods to play with a matrix
  
  ## Store the matrix in the cache
  
  setMatrix <- function(matrix = matrix()){
    x <<- matrix
    inverse <- NULL
  }
  
  ## Get the matrix from the cache
    
  getMatrix <- function() x
  
   ## Store the Inverse matrix in the cache
  
  setInverse <- function(inverseMatrix = matrix()){ 
    inverse <<- inverseMatrix
  } 
  
  ## Get the inverse matrix from the cache
  
  getInverse <- function() inverse
  
  ## Assign the method to the function
  
  list(get = getMatrix, set = setMatrix, getInverse = getInverse, setInverse = setInverse)
}


## checks if the given makeCacheMatrix object already has it's inverse computed.
## Otherwise, it calculates the inverse Matrix and caches it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  
  if(is.null(x$getInverse())){
    print("Inverse matrix does not exists in cache. Computing...") 
    x$setInverse(solve(x$get()))
    print("Inverse matrix computed and cached.") 
  }else {print("Inverse Matrix from cache")}
  
  x$getInverse()
}
