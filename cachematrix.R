# We are creating an object in which we will be able to store
# a matrix and the corresponding inverse matrix. This way we
# can cache the inverse matrix in order to save time
# and computationall time.
# The object content methods (functions) to set and get both matrices.
# The second function receive an makeCacheMatrix object and recover
# the inverse matrix (or calculate it if don't cached)
# Example of how to use:
# We can create a makeCacheMatrix mat with the sentence:
# mat<-makeCacheMatrix()
# We can assign a matrix calling the function set:
# mat$set(matrix(sample(25),5))
# And later we can view the matrix stored:
# mat$get()


## Write a short comment describing this function
#***************** makeCacheMatrix ***************
#Receive: x -> square invertible matrix
#Returns: A list with four methods/functions 
#         (set, get, setinverse and getinverse)
#The function create a object to store a matrix
#and the corresponding inverse matrix
#and have methods for setting and getting these matrices
#*************************************************
makeCacheMatrix <- function(x = matrix()) {
  #m will store the inverse matrix.
  m <- NULL
  #set function will be used to store the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get function will be used to show the matrix stored
  get <- function() x
  #setinverse will be used to store the inverse matrix
  setinverse <- function(inverse) m <<- inverse
  #getinverse will be used to show the inverse matrix
  getinverse <- function() m
  
  #Return a list of the functions as result of this function
  #This way we will be able to call this functions.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

#***************** makeCacheMatrix ***************
#Receive: x -> a makeCacheMatrix object
#Returns: m -> inverse matrix (either calculated or cached)
#This function use a makeCacheMatrix object. Search for a cached
#inverse matrix and returns it. If there is not a cached matrix
#then it calculate the inverse matrix, store it in the 
#makeCacheMatrix and return the matrix.
#**************************************************

cacheSolve <- function(x, ...) {
  #We get the inverse matrix
  m <- x$getinverse()
  if(!is.null(m)) { #if cached
    message("getting cached data") 
    return(m) #return the matrix and exit the function
  }
  #if not cached
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m) 
  m
}
