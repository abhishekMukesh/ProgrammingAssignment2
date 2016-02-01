## The functions below create a list of functions over a matrix and then caches the inverse globally for this object which gets retrived if
## if called cachesolve is called on same object twice, possible way to test :
## L<-makeCacheMatrix(matrix(c(1,2,3,0,1,4,5,6,0),3,3,TRUE))
## cacheSolve(L)   #Should give the inverse matrix
## cacheSolve(L)   #Should display message "getting cached data" then display the inverse

##makeCacheMatrix builds a list of pointers to functions to its argument(x) 
##in the current scenario the set function is not used, however it can be used to reset x from within cachesolve using the ... argument
##get function returns the value of x from the scope of this function
##setmatrix sets m to its argument globally
##getmatrix returns the value of m (which is in global scope due to setmatrix)

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}


## This function takes as input a list of functions from the previous function makeCacheMatrix called on a matrix,
##if m is previously set using this function then it simply returns the value from context else it computes and sets m to the inverse
##inside the object created by makeCacheMatrix, thus calling it repeatedly on same list causes cached data to be read.

cacheSolve <- function(x, ...) {
        m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
