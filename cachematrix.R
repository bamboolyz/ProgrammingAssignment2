## makeCacheMatrix has two variables, x and m 
## x is a matrix vector
## m is used to store the value after the processing

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


## the variable x input-ed must be a makeCacheMatrix 

cacheSolve <- function(x, ...) {
## x return a makeCacheMatrix
## x$getinv uses the makeCacheMatrix vector's getinv function, confirm if the getinv is calculated before
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
