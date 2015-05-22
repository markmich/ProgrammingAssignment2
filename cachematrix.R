## These two functions cache the inverse of a matrix so that it can be reused
## without having to recompute it repeatedly if the contents of the original
## matrix have not changed

## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL			## initialize inverted matrix value
	set <- function(y) {
		x <<- y			## set matrix to new value
		inv_x <<- NULL	## nullify existing inverted value
	}
	get <- function() x		## return the matrix value
	setinv <- function(inverted) inv_x <<- inverted	
	        			## set the value of the inverted matrix
        getinv <- function() inv_x	## return the value of the inverted matrix
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve returns the inverse of the matrix return by makeCacheMatrix,
## checking first to see if the cached value is valid.  If not valid, it
## computes the inverse

cacheSolve <- function(x, ...) {
        inv_x <- x$getinv		## get stored inverse value
	if (!is.null(inv_x) {    	## if a valid value, then...
		message("getting cached value")
					## alert the user
		return(inv_x)	        ## return cached value
	}
	orig_matrix <- x$get	        ## get the original matrix
	inv_x <- solve(orig_matrix, ...)## compute its inverse
	x$setinv(inv_x)			## set the value
	inv_x   			## return it		
}
