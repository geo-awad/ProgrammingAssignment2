## makeCacheMatrix function creates an object which is a list of 4 functions
## to set,get data matrix and set and get its inverse.

## while cacheSolve function retrieves the cached inverse of the object or if 
## there is no cached object it sets(caches) a new object and its inverse and 
## return the value of the inverse.

#################################################
## This function sets (cached), and gets a matrix, and
## sets (cached) and gets its inverse

makeCacheMatrix <- function(x = matrix()) {
 
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) 
                {
                inv <<- inverse
                
        }
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#############################################
## Return a matrix that is the inverse of object x matrix

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        if(!is.null(m)) 
        {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$set(data)
        x$setinverse(m)
        
        m
                
}
