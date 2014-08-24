# The goal of this project is to create two functions to compute and cache matrix inversions.
# Each function is explained inline

# EXECUTION STEPS
#
# Step 0: Create a new square+invertible matrix t
# t = matrix(c(1,2,2,1),ncol=2,nrow=2)
#
# Step 1: Load this file as source
#> source("cachematrix.R")
#
# Step 2: Create a new cache matrix with any square+invertible matrix t
#> a <- makeCacheMatrix(t)
#
# Step 3: Solve for the first time. This returns the inverted matrix and updates the results in the cache
#> cacheSolve(a)
#			[,1]       [,2]
#	A -0.3333333  0.6666667
#	B  0.6666667 -0.3333333
#
# Step 4: Solve again. This returns the inverted matrix directly from the cache
#> cacheSolve(a)
#	getting cached data
#			[,1]       [,2]
#	A -0.3333333  0.6666667
#	B  0.6666667 -0.3333333
#


## The function makeVector creates a special "vector", which is really a list containing a function to:
##   1. set the value of the vector
##   2. get the value of the vector
##   3. set the value of the inverse
##   4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{
    # Initialize empty cache
    m <- NULL

    # Assign new value to matrix. Clear cache.
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }

    # return the cached matrix
    get <- function() x

    # Cache Inverse
    setinverse <- function(inverse) m <<- inverse

    # Return cached inverse
    getinverse <- function() m
    
    # Return a list of functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the special "vector" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) 
{
    # Try to the inverse of 'x' from cache
    m <- x$getinverse()

    # null m indicates that it's inverse is not cached
    # non-null m indicates that it's inverse is cached and it merely returns the cached value
    if(!is.null(m)) 
    {
        message("getting cached data")
        return(m)
    }

    # Retrieve cached matrix x
    data <- x$get()	
 
    # Compute inverse
    m <- solve(data, ...)

    # Cache inverse
    x$setinverse(m)

    # Return the inverse
    m
}
