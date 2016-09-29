## This function creates a special matrix object which cashes the inverse of matrix. 
## Basically it creates a list of functions for setting and getting matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<- function() x
        setinverse<-function(inverse) i<<-inverse
        getinverse<-function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve calculates the inverse of the matrix created with makeCacheMatrix function. 
## If the inverse already exists in cache (was already calculated), this function just retrieves it, instead of calculating it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        matr<-x$get()
        inverse<-solve(matr,...)
        x$setinverse(inverse)
        inverse
}
