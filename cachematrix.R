## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function( x = matrix()) {
	list (
		set = function(y)	{
			x <<- y
			invmtx <<- NULL
		},
		get = function() x
		,
		setinvmtx = function(inverse) invmtx <<- inverse
		,
		getinvmtx = function() invmtx
	)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...) {
	
	invmtx <- x$getinvmtx()
	if ( !is.null(invmtx) ) {
		print(" Getting the value from Cache")
		return(invmtx)
	}
	data <- x$get()
	inverse <- solve(data)
	x$setinvmtx(inverse)
	print("Calculating the inverse not taking from cache")
	inverse
}
