## Functions to define a "matrix" object, calculate its
## inverse, and cache the inverse for later reference


## This function creates the special "matrix" object
## Contains a list of functions for setting/getting
## the matrix & its inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse_x <- NULL
	set <- function(y) {
		x <<- y
		inverse_x <<- NULL
	}
	get <- function() {
		x
	}
	setinverse <- function(inverse) {
		inverse_x <<- inverse
	}
	getinverse <- function() {
		inverse_x
	}
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## This function calculates the inverse of the matrix
## defined above & caches it

cacheSolve <- function(x, ...) {
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	matrix <- x$get()
	inverse <- solve(matrix)
	x$setinverse(inverse)
	inverse
}
