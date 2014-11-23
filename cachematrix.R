## makeCacheMatrix: This function creates a special "matrix" object that
#  can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	# Create a placeholder
	inv <- NULL
	# Set the value of the matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	# Get the value of the matrix
	get <- function() x
	# Set the value of the inverse of the matrix
	setInverse <- function(inverse) inv <<- inverse
	# Get the value of the inverse of the matrix
	getInverse <- function() inverse
	# Create a list of the values calculated
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}
## cacheSolve: This function computes the inverse of the special "matrix" 
#  returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
#  then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	# Gets the inverse from makeCacheMttrix function
	m <- x$getInverse()
	# Checks to see if the inverse has already been calculated
	if (!is.null(m)) {
		print("getting cached data")
		# If so, it returns the inverse
		return(m)
	}
	# Otherwise it calculates the inverse
	m <- solve(x$get())
	x$setInverse(m)
	# And returns the value
	m 
}
