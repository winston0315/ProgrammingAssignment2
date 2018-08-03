## Put comments here that give an overall description of what your
## functions do

## create a lit containing 4 functions to
##1>set the value of matrix
##2>get the value of matrix
##3>set the value of the inverse of the matrix
##4>get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ivs<-NULL
    set<-function(y){
        x<<-y
        ivs<<-NULL
    }
    get<-function()x
    set_ivs<-function(solve) ivs<<-solve
    get_ivs<-function() ivs
    list(set=set,
         get=get,
         set_ivs=set_ivs,
         get_ivs=get_ivs)
}

## calculates the inverse of the special "matrix" created with 
## the "makeCacheMatrix"

cacheSolve <- function(x, ...) {
    ivs <- x$get_ivs()
    if(!is.null(ivs)) {
        message("getting cached data")
        return(ivs)
    }
    data <- x$get()
    ivs <- solve(data)
    x$set_ivs(ivs)
    ivs
}
