#Concept of Lexical Scopping


# makeCacheMatrix = This function will solve for 
makeCacheMatrix <- function(x = matrix()) {
        # create a matrix object x and some associated sub-functions/methods
        
        # define the cache m
        m <- NULL
        set <- function(y) {
                x <<- y 
                # assign the input matrix y to the variable x 
                
                m <<- NULL # re-set m in the parent  to null
        }
        get <- function() x # return the matrix x
        setinverse <- function(inverse) m <<- inverse 
        
        getinverse <- function() m #return the inversed cache of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


# cacheSolve function which check if  inverse has been already calculated , extracting it from the cache, or else, the inverse will be calculated and stored it for future usage. we need to do this because calculating the inverse of a matrix is computationally expensive

cacheSolve<- function(x, ...) {
        ## Return a matrix that is the inverse
        
        m <- x$getinverse()
        #If m is already calculated, don't recalculate, retrieve its value
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                
                
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        #Return m
        m
}
