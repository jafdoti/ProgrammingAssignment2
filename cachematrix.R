# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
    
    # Test here to ensure argument matrix is a square matrix
    # For this assignment, assume that the matrix supplied is always invertible.
    # if(nrow(x) != ncol(x)){
    #    message("This function requires a square matrix - a matrix with an equal number of columns and rows.")
    # }
    
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setInvMtrx <- function(inv) m <<- inv  
    
    getInvMtrx <- function() m
    
    list(set = set, get = get,
         setInvMtrx = setInvMtrx,
         getInvMtrx = getInvMtrx)        
    
    
}

# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
    
    m <- x$getInvMtrx()
    
    # Check if cached matrix exists/not null
    # If so, return matrix
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    else{
        
        # Get original matrix
        data <- x$get()
        
        # Calculate the inverse
        m <- solve(data, ...)
        
        # Store the inverse matrix
        x$setInvMtrx(m)
        
        m
    }    
}