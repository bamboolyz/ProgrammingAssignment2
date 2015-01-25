## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    set<- function (y){
        x<<-y
        m<<-NULL
    }
    inv<-function()x
    setinv<-function(solve) m<<-solve
    getinv<-function()m
    list(set=set, 
         inv=inv,
         setinv=setinv,getinv=getinv)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    m<-x$getinv()
    if(!is.null(m)){
            message("getting cached data")
            return(m)
    }
    data<-x$inv()
    m<-solve(data,...)
    x$setinv(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
