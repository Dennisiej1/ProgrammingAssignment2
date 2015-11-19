## -----------The following two functions are used to cache the inverse of a matrix--------------.

## makeCacheMatrix creates a list containing a function to create te inverse of a matrix
makeCacheMatrix <- function(matrix_var = matrix()) {
  #set the matrix value and give it to a variable
  temp_var <- NULL
  set <- function(y)
  {
    matrix_var <<- y
    temp_var <<- NULL
  }
  #get the matrix value and give it to a variable
  get <- function() matrix_var
  
  #set the value of inverse of the matrix and give it to a variable
  setinverse_var <- function(inverse_var) temp_var <<- inverse_var
  
  #set the value of inverse of the matrix and give it to a variable
  getinverse_var <- function() temp_var
  list(set = set, get = get, setinverse_var = setinverse_var, getinverse_var= getinverse_var)
}


## the function below returns the inverse of the matrix. If not, 

cacheSolve <- function(matrix_var, ...) {
  ## Return a matrix that is the inverse of 'matrix_var'
  temp_var <- matrix_var$getinverse_var()
  #check if the inverse has already been computed.
  if(!is.null(temp_var)) {
    message("getting the cached data")
    return(temp_var)
  }
  #computing the inverse, sets the value in the cache with the setinverse function.
  data <- matrix_var$get()
  temp_var <- solve(data, ...)
  matrix_var$setinverse_var(temp_var)
  temp_var
}

## An important part of programming is testing, in the lines below you can find my test results
# generate a random square, non-singular matrix
test <- matrix(runif(9,1,10),3,3)
# generate the makeCacheMatrix object with this matrix
testCached <- makeCacheMatrix(test)
# from now on calculate or retrieve calculated inversion using the cacheSolve function
testInv <- cacheSolve(testCached)
