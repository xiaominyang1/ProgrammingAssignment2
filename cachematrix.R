## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is a convoluted computation and there may be some advantages to 
## caching the inverse of a matrix rather than computing it over and over.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
## There exist four different functions, which are provided for inverse caching manipulation
## for example: set function was called whenever the matrix has been modified

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function (y){
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setSolve <- function (solve) s <<- solve
	getSolve <- function () s
	list(set = set, get = get, 
		 setSolve = setSolve,
		 getSolve = getSolve)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve

        if(!is.null(s)){
        	message("getting cached Solve")
        	return(s)
        }

        data <- x$get()
        s <- solve(x)
        x$setSolve(s)
	  s
}
