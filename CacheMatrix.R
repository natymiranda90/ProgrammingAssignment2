#Natasha Miranda (Week 3: ProgrammingAssigment2)

##The first function, MakeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
           inv1 <- NULL
           set  <- function(y){
                      X <<- y
                      inv1 <<- NULL
                      }
           get <-function()x
           setinverse<-function(inverse) inv1 <<- inverse
           getinverse<-Function() inv1
           list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The second function, cachesolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv1 <- x$getinverse()
        if(!is.null(inv)) {
                   message ("getting cached data")
                   return(inv1)
        }
        data <- x$get()
        inv1 <- solve(data, ...)
        x$setinverse(inv1)
        inv1
}
