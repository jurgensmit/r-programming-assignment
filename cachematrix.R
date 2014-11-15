## This module provides a special matrix for which
## the inverse is cached (i.e. only calculated once) 
## If new data is assigned to the matrix the cached inverse
## is cleared.
## To create the special matrix call the function makeCacheMatrix.
## To calculate the inverse of the matrix call the function 
## cacheSolve.

## This function creates a matrix for which the inverse
## is calculated only once.
## The special matrix has four functions:
##     set: set the data of the matrix
##     get: get the data of the matrix
##     setInverse: set the inverse of matrix which will be cached
##     getInverse: get the cached inverse of the matrix   
makeCacheMatrix <- function(x = matrix()) {
	cachedInverse <- NULL
	set <- function(y) {
		x <<- y
		cachedInverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) cachedInverse <<- inverse
	getInverse <- function() cachedInverse
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

## This function calculates the inverse of the matrix created
## by makeCacheMatrix. If the inverse has already been calculated
## before this function will return the cached inverse.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inverse <- x$getInverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setInverse(inverse)
	inverse	
}
