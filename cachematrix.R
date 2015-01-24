## Matrix Inversion is a resource heavy calculation
## The below functions help cache the inverse of a matrix
## If a matrix is passed that has already been inverted...
## ... the functions return the cached inverted matrix
## If a new matrix is passed, then its inverse is calculated...
## ... and cached for future use

## The makeCacheMatrix function takes a matrix as argument
## It returns a list of four functions:
##      setmatrix() sets the cache to the inverse of passed matrix
##      getmatrix() returns cached matrix
##      setinverse() saves new inverse in cache
##      getinverse() returns cached inverse
## Each of these functions can be called to access desired matrices

makeCacheMatrix <- function(initial_matrix = matrix()) {
    inverse_matrix <- NULL
    
    setmatrix <- function(M) {
        initial_matrix <<- M
        inverse_matrix <<- solve(M)
        }
    
    getmatrix <- function() {
        initial_matrix
    }
    
    setinverse <- function(inverted_matrix) {
        inverse_matrix <<- inverted_matrix
    }
    
    getinverse <- function() {
        inverse_matrix
    }
    
    list(setmatrix = setmatrix, getmatrix = getmatrix, 
         setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is a "wrapper" function around makeCacheMatrix
## cacheSolve checks if the inverse of passed matrix has been cached
## If cached inverse is not found, a new inverse is calculated and cached
## If cached inverse is found, it is returned (thus saving computation)
## cacheSolve also displays a message when a cache is being retrieved

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    cached_inverse <- x$getinverse()
    
    if(!is.null(cached_inverse)) {
        message("getting cached inverse...")
        return(cached_inverse)
    }
    
    mat <- x$getmatrix()
    inv <- solve(mat, ...) 
    
    x$setinverse(inv)
    inv
    
}
