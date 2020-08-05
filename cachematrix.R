#This pair of functions can create a special matrix object that caches its inverse.

#makeCacheMatrix() – create a cache for the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {	#x is by default an empty matrix
	inv <- NULL
	set <- function(y) {				
	x <<- y					#assign input argument to the x object in parent env
	inv <<- NULL					#assign value of NULL to inv object in parent env.
	}
	get <- function() x				#R retrives value of x from the parent env
	setsolve <- function(solve) inv <<- solve	#R retrives value of inv from the parent env
	getsolve <- function() inv
	list(set = set, get = get,			#returns a list where each element is named
		setsolve = setsolve,
		getsolve = getsolve)
}

# cacheSolve() - retrieve the inverse of an object of type makeCacheMatrix()

cacheSolve <- function(x, ...) {
	inv <- x$getsolve()				#attempts to retrive an inverse from object
	if(!is.null(inv)) {				#checks if the result is NULL. 
		message("getting cached data")	#if not NULL, return the cached inverse to parent env
		return(inv)
	}
	data <- x$get()				#if NULL, cacheSolve gets the input object,
	inv <- solve(data, ...)			#calculates the inverse,
	x$setsolve(inv)				#sets the inverse in the input object
	inv						#and returns the inverse to the parent env
}
