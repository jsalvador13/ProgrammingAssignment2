## These functions are part of the Programming Assignment 2 of the
## R Programming course in Coursera. The aim of these functions is
## to cache potentially time-consuming computations

## The function makeCacheMatrix creates a special matrix object that
## can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list (set=set, get=get,
		  setinverse=setinverse,
		  getinverse=getinverse)
}


## The function cacheSolve computes the inverse of the special matrix
## returned by the makeCacheMatrix function. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
        	message ("getting cache data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
