## Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

#makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        setMatrix <- function(y) {        
                x <<- y 
                m <<- NULL
        }
        
        getMatrix <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(setmatrix = setMatrix, getmatrix = getMatrix, setinverse = setInverse, getinverse = getInverse)
}


##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed),
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(nrow(x$getmatrix())==ncol(x$getmatrix())) {   #condition to check square matrix
                m <- x$getinverse() 
                if(!is.null(m)){ 
                    message("getting cached data.....")
                    return(m)
                }
                y <- x$getmatrix() 
                m <- solve(y)
                x$setinverse(m) 
                m   # returns the inverse matrix
        }
        else 
                 print("***************** please pass a square matrix as an argument ******************")
}

## Sample run:
## m <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)
## m2 <- makeCacheMatrix(m)
## No cache in the first run
##cacheSolve(m2)
## Retrieving from the cache in the second run
#cacheSolve(m2)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
##getting cached data.....
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
