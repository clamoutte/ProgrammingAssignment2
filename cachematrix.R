## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix: 
#This function creates a special "matrix" object 
#that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  #initialize inverse
  i <- NULL
  #set matrix value
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #get matrix value
  get <- function() x
  
  #set matrix inverse value using solve function
  #and cache in i variable
  setinverse <- function(solve) i <<- solve
  
  #get cachecd value of inverse stored in i
  getinverse <- function() i
  
  #return list 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
##cacheSolve: This function computes 
##the inverse of the special "matrix" 
##returned by makeCacheMatrix above.
##If the inverse has already been calculated 
##(and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #check if inverse is in cache and if so return
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  #if inverse has not been calculated, do so by calling
  #set inverse
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
