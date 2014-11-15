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
	# initialize the cache
	cachedInverse <- NULL
	
	# store the matrix
	set <- function(y) {
		x <<- y
		# clear the cached inverse
		cachedInverse <<- NULL
	}
	
	# get the matrix
	get <- function() x

	# store the cached inverse
	setInverse <- function(inverse) cachedInverse <<- inverse

	# get the cached inverse
	getInverse <- function() cachedInverse
	
	# return the functions that can be called on this matrix
	list(
		set = set, 
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

## Return a matrix that is the inverse of 'x'
## This function calculates the inverse of the matrix created
## by makeCacheMatrix. If the inverse has already been calculated
## before this function will return the cached inverse.
cacheSolve <- function(x, ...) {
	# try to retrieve the cached inverse
	inverse <- x$getInverse()

	if(!is.null(inverse)) {
		# if the cached inverse is available return it
		return(inverse)
	}

	# if the cached inverse is not yet available, calculate it
	data <- x$get()
	inverse <- solve(data, ...)
	
	# cache the inverse 
	x$setInverse(inverse)

	# return the result
	inverse	
}
