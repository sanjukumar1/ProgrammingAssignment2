## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## library MASS is used to get inverse of squared as well as non squared matrix
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<- function(y){
        x<<-y
        inv<<-NULL 
    }
    get<-function()x  ##to get matrix x
    setinv <- function(inverse) inv <<- inverse 
    getinv <- function() {
        
        inver<-ginv(x)
        inver%*%x
    }
    list(set = set, get = get,
         setinv = setinv,
         getinverse = getinv)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
    inv<-x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }###if close
    data<-x$get()
    inv<- solve(data....)
    x$setinv(inv)
    inv
}

