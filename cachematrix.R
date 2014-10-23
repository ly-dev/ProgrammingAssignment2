## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( x = matrix() ) {

	## Initialize the inverse property
    i <- NULL

    ## Method to set the matrix
    set <- function( matrix ) {
            x <<- matrix
            i <<- NULL
    }

    ## Method the get the matrix
    get <- function() {
    	## Return the matrix
    	x
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix created by "makeCacheMatrix"
## If the inverse has already been calculated, then the "cacheSolve" 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(inverse) ) {
            message("getting cached data")
            return(inverse)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    inverse <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(inverse)

    ## Return the matrix
    inverse
}