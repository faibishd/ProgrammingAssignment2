## Put comments here that give an overall description of what your functions do
# The functions take a matrix and calculate its inverse. The inverted matrix is cached. 
# If that inverted matrix was already calculated, the result is taken from the cache.

## Write a short comment describing this function
# This function contains a list of functions that implement the logic of the assignment
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                              # set the value m to NULL to initialize it
  set <- function(y) {                   # create value "set" to store function y 
    x <<- y                              # x gets the value y
    m <<- NULL                           # m gets NULL
  }
  get <- function()x                     # create function 'get' and assign matrix x to it  
  setinver <- function(solv) m <<- solv  # assigns value of inverted matrix to m
                                         # in the makeCacheMatrix frame 
  getinver <- function() m               # returns value of m from the makeCacheMatrix frame 
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}

## Write a short comment describing this function
# This function gets a matrix as input, and checks if there is an inverted matrix associated with this matrix
# If the inverted matrix was already calculated, the inverted matrix is returned from the cache, otherwise it is calculated

cacheSolve <- function(x, ...) {
                                         ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinver()                      # go to the 'x' environment > get the value of m from that
                                         # environment and assign it to m in the current environment 
  if(!is.null(m)){                       #if m was previously calculated
      message("getting cached data")     
      return(m)                          #return the previously calculated inverted matrix
  }                                      #if m was not previously calculated:
  data <- x$get()                        #get the matrix from the 'x' environment
  m <- solve(data, ...)                  #calculate the inverse of the matrix in the local 'data' variable
  x$setinver(m)                          #store the inverse of the matrix in the 'x' environment
  m
}
