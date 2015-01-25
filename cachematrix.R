## makeCacheMatrix is a function used to create a special matrix object that can cache its inverse. It has two variables, x and m 
## x is a matrix vector
## m is used to store the value after the processing

makeCacheMatrix <- function(x = matrix()) {
    ## sets x equal to an empty matrix
    m<- NULL
    ## sets the stored value (the inverse) as m variable equal to NULL
    set<- function (y){
        x<<-y
        ## set function assignes the argument to x
        m<<-NULL
        ## once the the set function is called, inverse is re-set to NULL
    }
    get<-function()x
    ## get function returns the matrix
    
    setinv<-function(solve) m<<-solve
    ## setinv overrides the previous value of m and assigns argument to inverse
    
    getinv<-function()m
    ## getinv returns the inverse
    
    list(set=set, 
         get=get,
         setinv=setinv,getinv=getinv)
    ## creates list of functions
}


## the variable x input-ed must be a makeCacheMatrix 
## this function calculate the inverse of the special matrix. 
## if inverse calculated, then cachesolve retrive the inverse cached value.

cacheSolve <- function(x, ...) {
## x return a makeCacheMatrix
## x$getinv uses the makeCacheMatrix vector's getinv function, confirm if the getinv is calculated before
    m<-x$getinv()
    if(!is.null(m)){
            message("getting cached data")
            return(m)
            ## If the value of Inverse is NOT null (was previously calculated), cacheSolve returns that value 
    }
    ## If the value of Inverse is NULL, then retrive matrix x and calculate the inverse with the solve() function
    data<-x$inv()
    m<-solve(data,...)
    x$setinv(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
