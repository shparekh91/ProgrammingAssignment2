# The following two functions are used to cache the inverse of a matrix which can be 
# beneficial in avoiding repeated computation in performing inversion of a matrix.

# makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
# It contains the list of fucntions to set the matrix and its inverse, also to get that matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) 
{
        ## Initialize the inverse property
        inv <- NULL
        
        ## Method to set the matrix
        set <- function(y) 
        {
                x <<- y
                inv <<- NULL
        }
        
        ## Method the get the matrix        
        get <- function() x
        
        ## Method to set the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        
        ## Method to get the inverse of the matrix
        getinverse <- function() inv
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated then the it should retrieve the inverse from the cache. 
# If not, it computes the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        ## Just return the inverse if its already set
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        
        ## Get the matrix from our object
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        inv <- solve(data)
        
        ## Set the inverse to the object
        x$setinverse(inv)
        
        ## Return the matrix
        inv
}
