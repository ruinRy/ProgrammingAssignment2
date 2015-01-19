## Author: Ruyi Wang
## Date: 01/18/2015
## Filename: cachematrix.R 
## Function: Create an object that can cache the inverse of a matrix.
## CacheSolve can provide the inverse from the cache or calculate a 
## new inverse and store it into the cache.

## makeCacheMatrix is a vector of function
## get() provides the matrix itself 
## set() sets a new matrix or make any change to the matrix
## getsolve() provides the inverse of matrix in the cache
##			  NULL if there is nothing in cache
## setsolve() stores the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(inverse) m <<- inverse
	getsolve <- function() m
	list(set = set, get = get, 
		 setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve returns the inverse of the matrix of vector x
## First it tries to get the inverse in the cache.
## If inverse has not been calculated, calculate the inverse
## by "solve(data)" and store it in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	
	m <- x$getsolve()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setsolve(m)
	m   
}
