## These two functions cache the inverse of a matrix
## function name is changed to make the things clearer

## This function store the raw data as well as the inverse

makeCacheMatrix <- function(data = matrix()) {
        result <- NULL          
        setdata <- function(y) {         ## function for setting the raw data
                data <<- y
                result <<- NULL
        }
        getdata <- function() data       ## function for getting the data
        
        ## function for storing the inverse in the cache
        setinverse <- function(inverse) result <<- inverse 
        
        ## function for geeting the inverse
        getinverse <- function() result
        list(setdata = setdata, getdata = getdata,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function return the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()          ## Get the cached inverse
        if(!is.null(inv)) {            ## return cached value if available
                message("getting cached data")
                return(inv)
        }
        data <- x$getdata()            ## if cached not available, get raw data
        inv <- solve(data, ...)        ## calculate the inverse again
        x$setinverse(inv)              ## store the inverse
        inv
}

## This function test the code.
TestCode <- function() {
        a <- matrix(as.numeric(1:4),2)   ## Test matrix.
        OriginalMat <- makeCacheMatrix(a)  
        cacheSolve(OriginalMat)  ## No cached inverse
        cacheSolve(OriginalMat)  ## get the cached inverse
        
        CachedInverse <- OriginalMat$getinverse()   
        
        ## Test whether the inverse is made properly
        ## Should return identity matrix if the code is correct
        OriginalMat$getdata() %*% CachedInverse  
}
