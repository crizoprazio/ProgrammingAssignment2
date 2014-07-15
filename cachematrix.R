## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The function makeCacheMatrix extracts the values of the matrix,
#and cache its inverse.

myMx<-matrix(c(5,3,8,7),2,2)     #creates a matrix called myMx

#This matrix is invertible, which can be checked with the dev function 
#If the determinant of the square matrix is different to zero, then the 
#matrix is invertible

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL           #initialize the cached matrix to NULL
        setMx<-function(y){
                x<<-y      #set the internal matrix to the passed argument
                m<<-NULL   #reset the cached matrix
        }
        getMx<-function()x  #get the original matrix
        setIMx<-function(solve)m<<-solve    #set the cached inverse matrix             
        getIMx<-function()m         #return the cached inverse matrix
        
        return(list(setMx=setMx,getMx=getMx,
                    setIMx=setIMx,
                    getIMx=getIMx)) #return the list of functions available
}


## Write a short comment describing this function
# The function cacheSolve retrieves the inverse of the matrix
#computed by makeCacheMatrix only if the inverse has already been 
#calculated

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        scache<-x$getIMx()
        
        if(!is.null(scache)){
                message("obtaining cached mean")
        }
        else{
                message("processing and caching inverse matrix")
                data<-x$getMx()
                scache<-solve(data, ...)
                x$setIMx(scache)
        }
        return(scache)
        
}