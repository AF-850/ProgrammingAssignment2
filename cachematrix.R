## This function creates a cashe (or storage) for storing the inverse values of a given matrix (in getinv)
makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# This function calcualtes the inverse of a matrix after checking whether it has been already 
# calculated and stored in the cashe (getinv) or not; if so, there is no need to recalculate it 
# and the function returns the stored value

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
  # m is the inverse of the given matrix 
}

