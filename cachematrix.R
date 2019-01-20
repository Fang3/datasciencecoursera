## Put comments here that give an overall description of what your
## functions do
#The pair of functions create then compute/store the inverse of a given matrix.
#The makeCacheMatrix function creates a matrix object that contains the value of a given matrix and its inverse along with 
#basic mutator and accessor for those data fields.
#The cacheSolve function serves as a lookup table to identify the inverse of a given matrix object returned by the 
#call to makeCacheMatrix, it will return the inverse if it is not NULL (not computed). Otherwise, the function will
#compute the inverse of the matrix then store it using the setter. 

## Write a short comment describing this function
#The mackeCahcheMatrix function creates a matrix object with setter and getter along with its values and inverse 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
#The cacheSolve function check if the inverse is already computed. If it is, then return the stored invese
#If not, then compute and return/set the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}


