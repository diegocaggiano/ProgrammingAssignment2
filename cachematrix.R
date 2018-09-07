## These functions calculate the inverse of a given invertible matrix
## using the cache to avoid wasting computing resources

## Function "makeCacheMatrix" retrieves a list of functions to apply to input matrix
## we assume input matrix is always invertible
makeCacheMatrix <- function(input_matrix = matrix()) {
  inverse_matrix <- NULL
  
  # set function
  set <- function(y) {
    input_matrix <<- y
    inverse_matrix <<- NULL
  }
  
  # get function
  get <- function() input_matrix
  
  #set inverse matrix function
  set_inverse <- function(m_inverse) inverse_matrix <<- m_inverse
  
  # get inverse matrix function
  get_inverse <- function() inverse_matrix
  
  # output list
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Function "cacheSolve" retrieves the inverse of matrix from cache in case it 
## has already been calculated. Otherwise, inverse is calculated
cacheSolve <- function(x, ...) {
        
  m <- x$get_inverse()
  if(!is.null(m)) {
    # returning cached data
    print("returning cached data")
    return(m)
  }
  else {
    # if there is no cached data, let's calculate the inverse matrix
    data <- x$get()
    m <- solve(data, ...)
    # let's keep the matrix in the cache for future calls
    x$set_inverse(m)
    # returning recently calculated inverse matrix
    return(m)
  }
}
