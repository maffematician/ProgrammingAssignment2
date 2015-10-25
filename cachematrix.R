
## Create a matrix, calculate its inverse, and store the
## inverse for fast retrieval


## First make a function that has hooks for storing the
## matrix, retrieving the matrix, storing its inverse, and
## retrieving the inverse.


makeCacheMatrix <- function(x = matrix()) {

    ## stop now if the input is not a matrix
    if (!is.matrix(x)) {
    	stop("Argument must be a matrix")
  	}

    ## initialize an empty matrix object to store the inverse
    mxInverse <- NULL

    ## make a function storing the matrix
    ## make sure to flush any inverse already stored
    set <- function(y) {
        x <<- y
        mxInverse <<- NULL
    }
    
    ## make a function just for retrieving the matrix
    get <- function() x

    ## make a function for storing the inverse
    setinverse <- function(solve) mxInverse <<- solve
    
    ## make a function just for retrieving the inverse
    getinverse <- function() mxInverse

    ## publish everything in a list with anchors
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

;

## Now make a function that solves a matrix for its
## inverse, using the hooks into "makeCacheMatrix"
## to store & retrieve everything. Don't calculate
## the inverse if it's already cached.

cacheSolve <- function(x, ...) {

    ## first take the stored inverse from the parent
    mxInverse <- x$getinverse()
    
    ## if the stored inverse is not NULL, use it
    if(!is.null(mxInverse)) {
        message("getting inverse from cache")
        return(mxInverse)
        ## "return" will exit program
    }
    
    ## assuming we are still processing at this point,
    ## get the original matrix from the parent function
    data <- x$get()

    ## solve for the inverse
    mxInverse <- solve(data, ...)

    ## store the inverse we just calculated
    x$setinverse(mxInverse)

    ## return the inverse
    mxInverse
}
