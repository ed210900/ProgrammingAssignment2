## Below are two functions used to create a special object that stores a matrix 
##and cache's its inverse matrix

##initialise a matrix in global environment 
makeCacheMatrix <- function(x = matrix()){
    cache <- NULL                     ##Initiation of first variable set to NULL
    set <- function(y){             ##Initiation of a function.  Function allows user to
        x <<- y                      ##to reset the matrix numbers
        cache <<- NULL
    }
    
    get <- function() x                                  ##Initiation of a function which allow you to get or see the output
    setInverse <- function(inverse) cache <<- inverse     ##Initiation of a function which stores the value of 
                                                    ##inverse of the matrix into cache
    
    getInverse <- function() cache         ##Initiation of a function which allow you get or
                                            ##see the output of the matrix  called cache
   
    list(set = set, get = get,             ##Lists all functions into a list
         setInverse = setInverse,
         getInverse = getInverse)
}



## The following function calculates the inverse of the special "matrix" 
##created with the above function. It first checks to see if the 
##inverse has already been calculated. If so, it gets the inverse from the cache 
##and skips the computation. Otherwise, it calculates the inverse of the matrix and 
##sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...){
    
    ##Return a matrix that is the inverse of 'x'
    cache <- x$getInverse()
    if (!is.null(cache)) {
        message("getting cached data")
        return(cache)
    }
    data <- x$get()
    cache <- solve(data, ...)
    x$setInverse(cache)
    cache   ## Returns a matrix that is the inverse of 'x'
}
