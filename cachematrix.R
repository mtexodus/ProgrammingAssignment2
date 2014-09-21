## this function takes a matrix as an argument and returns a lsit containing four 
## functions 1.set() ,2.get() ,3.setInverse(),4getInverse()

## This function caches the matrix in memory and set the value of inverse to NUll
##in the initial call

makeCacheMatrix <- function(x = matrix()) {
    inv <-NULL
    set <- function(y){
        x<<-y
        inv<-NUll
    }
    get <-function() x
    
    setInverse <- function(inverse) inv <<-inverse    
    getInverse <-function() inv
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## this takes the list returned by the makecacheMatrix and checks if the inverse of matrix 
##already exists or not in memory , if the inverse in already cached then it retuns it
##else it calculates the inverse and caches it for future use.

cacheSolve <- function(x, ...) {
    inv<-x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mt<-x$get()
    inv<-solve(mt)
    x$setInverse(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
}
