## makeCacheMatrix and cacheSolve functions together 
## cache a matrix's inverse and returns it when needed

## makeCacheMatrix creates a list that contains several 
## functions that will halp cacheSolve function to cache
## the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
		##initialize the inverse matrix
		s<-NULL   
		set<-function(y){
			x<<-y
			s<<-NULL
		}
		##Assign the matrix to 'get'
		get<-function() x   
		##assign 'solve' to 's'
		setsolve<-function(solve) s<<-solve  
		## assign the inverse to 'getsolve'
		getsolve<-function() s  
		## create list with all four elements
		list(set = set, get = get,
		    setsolve = setsolve, getsolve = getsolve)

}


## cacheSolve function checks to see if the inverse is
## cached and returns the inverse if inverse is not cached
## this function will calculate and return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	## Assign Cached inverse to s
	s<-x$getsolve()

	## Check to see if the inverse is infact exists in cache
	## Tell the user the cache exists
	## Return the cached inverse
	if(!is.null(s)){
		message("getting cache data")
		return(s) 
	}
	##if inverse is not cached, matrix is assigned to variable data
	data <- x$get()
	
	##Matrix inverse is calculated here
	s <- solve(data, ...)
	
	##Inverse matrix is assigned s or calculated inverse is cached
	x$setsolve(s)
	
	##Return the inverse matrix
	s
}
