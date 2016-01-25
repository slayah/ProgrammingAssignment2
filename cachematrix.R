## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Similar to makeVector function given in the example
#get and set functions return and set the matrix respectively
#get_inverse and set_inverse functions return and set the inverse of the matrix respectively
makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(i) inv<<-i
  get_inverse <- function() inv 
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
# It calls the solve() function for calculating inverse when it's not cached
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set_inverse(inv)
  inv
}

# Testing the code
#a <- makeCacheMatrix( matrix(c(1,4,3,4,5,6,7,8,9), nrow = 3, ncol = 3) );
#a$get()
#cacheSolve(a)


