## Put comments here that give an overall description of what your
## functions do
## This function consists of 2 major functions with sub functions. The major functions identifed 
## are makeCacheMatrix and cacheSolve. these functions allow us to set a matrix and retrieve the inverse of that matrix

## 

## Write a short comment describing this function
##makecacheMatrix has 4 sub functions and allows the user to define the matrix and retrun the inverse of the matrix. 
## These sub functions are 
## set : this allows user to define the matrix
## get: helps user identify the matrix 
## setinv : solves the inverse of the matrax the first time
## getinv: gets the inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(z) {
    x <<- z
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  

}


## Write a short comment describing this function
## this function attempts to identify whether previous calculation of the matrix is cached 
## if it is cached it returns the value immediately without further computation
## if it is not cached it calculates the inverse and 
## stores and set the environment variables to "cache" the answer
## it is now cached and if the request is performed again the cache answer is returned
## without additional computation
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
  
}
