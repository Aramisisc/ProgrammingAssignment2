#create a function that creates a first special object that stores a matrix 
# and cache's its inverse.
#The first function, makeCacheMatrix creates a special "vector", which is really 
#a list containing a function to:
#-set the value of the matrix
#-get the value of the matrix
#-set the the inverse matrix
#-get the inverse matrix
#We assume that the provided matrix is a square invertible matrix 
#f.i.: matrix(1:4,2,2)

makeCacheMatrix <- function(x = matrix())  {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#The second function,cacheSolve(), calculates the inverse of the matrix that 
#has been created with the makeCacheMatrix function. 
cacheSolve <- function(x, ...) {
        m <- x$getinverse() #checks to see if the inverser has already been 
                            #calculated
        if(!is.null(m)) {  #If so, it gets the inverse from the cache and skips
                           #the computation
                message("getting cached data") 
                return(m)
        }
        data <- x$get()  #Otherwise, it calculates the inverse of the matrix... 
        m <- solve(data, ...)
        x$setinverse(m)  #..and sets the value of the inverse in the cache via 
                         #the setinverse function
        m
}