## The two functions below, makeCacheMatrix and cacheSolve help speed up 
## computational time by storing the inverse of a matrix and returning this 
## whenever needed. This is similar to how most caches work. In order to 
## achieve the inteded result the inverse of the matrix will be stored
## in a global variable. If this value is not yet calculated the function
## will first compute this and then return the value



## The first function creates a special "vector" which contains a list with
## 4 special functions
##    - set the value of the matrix
##    - get the value of the matrix
##    - set the value of the inverse matrix
##    - get the value of the inverse matrix 
makeCacheMatrix <- function(x = matrix()) {
        
        xInverse <- NULL
        
        set <- function(y) {
                
                x <<- y
                xInverse <<- NULL
                
        }
        
        get <- function() x
        
        setinverse <- function(inverse) xInverse <<- inverse
        
        getinverse <- function() xInverse
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        

}


## The following function calculates the inverse of the special "vector" created above
## However, it first checks to see if the inverse has already been calculated.
## If it has then it returns the value from the cache otherwise it computes
## and stores it


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getinverse()
        
        if(!is.null(inverse)) {
                message("Getting cached data")
                return(inverse)
        }
        
        data <- x$get()
        
        inverse <- solve(data, ...)
        
        x$setinverse(inverse)
        
        inverse
        
        
}
