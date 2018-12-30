## Caching the Inverse of a Matrix


## Create the Cache of the Matrix

makeCacheMatrix <- function(x = matrix()) {

	invrs <- NULL
	set <- function(y) {
		x <<- y
		invrs <<- NULL
	}
	get <- function()x
	setInvrs <- function(inverse) invrs <<- inverse
	getInvrs <- function() invrs
	list(set = set, get =get,
	     setInvrs = setInvrs,
	     getInvrs = getInvrs)

}


## Check & Return matrix inverse if necessary

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invrs <- x$getInvrs()
	if(!is.null(invrs)){
		message("getting cached data")
		return(invrs)
	}
	matrx <- x$get()
	invrs <- solve(matrx,...)
	x$setInvrs(invrs)
	invrs
}
