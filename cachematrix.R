# makeCacheMatrix function
makeCacheMatrix <- function(X = matrix())
{
	# Initialize local inverse variable
	M <- NULL

	# Set function
	set <- function(Y)
	{
		X <<- Y
		M <<- NULL
	}

	# Get function
	get <- function()
	{
		return(X)
	}
	
	# Set inverse
	setinverse <- function(inverse)
	{
		M <<- inverse
	}
	
	# Get inverse
	getinverse <- function()
	{
		return(M)
	}
	
	# List of available functions
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}

# cacheSolve function
cacheSolve <- function(X, ...)
{
	# First call to getinverse
	M <- X$getinverse()
	
	# Check if inverse exists
	if( !is.null(M) )
	{
		# Inverse is cached, return cached value
		message("Getting cached data.\n")
		# Return cached value and get out of function
		return(M)
	}
	
	# Inverse does not exist, get matrix from input
	data <- X$get()
	# Find inverse matrix
	M <- solve(data, ...)
	# Set inverse matrix
	X$setinverse(M)
	# Return inverse
	return(M)
}