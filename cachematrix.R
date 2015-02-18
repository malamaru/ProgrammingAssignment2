## Contains two functions "makeCacheMatrix" and "cacheSolve"

## Create matrix type/object that can cache its inverse
## Function makeCacheMatrix used to cache the result for the contents of a result
## vector that are not changing can be looked up in the cache rather than recomputed
makeCacheMatrix <- function( m = matrix() )
{ # Initialise the inverse property
    inv <- NULL
    
    # Set the matrix
    set <- function( matrix )
    { mtrx <<- matrix
      inv  <<- NULL }
    
    # Get the matrix
    get <- function() 
    { # Return the matrix 
        mtrx }
    
    # Set the inverse of the matrix
    setInverse <- function(inverse)
    { inv <<- inverse }
    
    # Get the inverse of the matrix
    getInverse <- function()
    { # Return the inverse property
        inv }
    
    # Return cachedmatrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
} #end of function makeCacheMatrix


cacheSolve <- function(x, ...) {
    ## Compute the inverse of the matrix returned by "makeCacheMatrix". If 
    ## the inverse has been already calculated and the matrix has not been
    ## changed, then the "cachesolve" will retrieve the inverse from the cache
    
    # Return a matrix that is the inverse of 'x'
    mtrx <- x$getInverse()
    
    # Check if matrix has already been cached & return the inverse set
    if( !is.null(mtrx) )
    { message("getting cached data")
      return(mtrx) }
    
    # Get the matrix from argument object
    data <- x$get()
    
    # Calculate the inverse using matrix multiplication
    mtrx <- solve(data) %*% data
    
    # Set the inverse to the object
    x$setInverse(mtrx)
    
    # Return the inversed matrix
    mtrx
} # end of function cacheSolve