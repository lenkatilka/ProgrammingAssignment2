## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
## makeCacheMatrix sets the cach value of m and x with set function
## in the case when x was changed; the new value of x
## and m are set to cache memory (m is replaced by NULL) and
## then the inverse is computed and set to m with setinv function.
## Functons get and getinv retrieve values of x and m.


makeCacheMatrix <- function(x = matrix()) {

## Assigning the NULL to  m matrix (not to cached value yet)
    m<-NULL

## Function set sets the cache memory of matrix x to new value 
## and replace the old cached value of m to NULL 
    set<-function(y){
## Setting cach value of x
        x<<-y
## Setting the cache value of m to NULL (this is for the case
## when the matrix x is changed and we would need to recompute
## the inverse and store new inverse value to m)
	m<<-NULL
    }

## Get function retrieves the (cached) value of x
    get<-function() x
## Function setinv computes the inverse and stores it to
## cache value of m
    setinv<-function(solve) m<<-solve
## Function getinv retrieves the value of m
    getinv<-function() m
    list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## Write a short comment describing this function
## The following checks if the inverse of matrix was 
## computed already. If it was, then this inverse is
## retrieved from memory. If it was not, it is computed
## by calling x$setinv(m) function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

## retrieve the inverse of x
    m<-x$getinv()
## if inverse was computed (m is not NULL), than return cached value of m
## and exit this function
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
## retrieve value of x 
    data<-x$get()
## computed the inverse
    m<-solve(data,...)
## put inverse into cache
    x$setinv(m)
    m
}
