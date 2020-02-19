## Writing (2) functions that will create a matrix inverse and then store it
## memory to be retreived as cache for further processing. This way, matrix incerse will 
## not perform inverse calculation over and over again

## This function is part 1 of the assignment, to create a matrix and produce list 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ##m is set to NULL, initializing it as an object within the 
  ## makeCacheMatrix() environment to be used by later code in the function
  set <- function(y){ ##Assign the input argument to the x object in the parent environment
    x <<- y
    m <<- NULL ##Clear previous value of m 
  }
  get <- function() x
  setInv <- function(Inverse) m <<- Inverse
  getInv <- function() m
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## This function computes Inverse of the matrix defined in above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)){ ##check to see if result is NULL
    message("getting cached data")
    return(m) ## return the value
  }
  mat <- x$get() ## Matrix value from get()
  m <- solve(mat, ...) ## Caluclating Inverse
  x$setInv(m) ## Setting Inverse
  m
}
