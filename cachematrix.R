## These two functions allow to caching the inverse of a matrix rather than computing it repeatedly.

## makeCacheMatrix: Function to create a special matrix to allow caching its inverse

makeCacheMatrix <- function(x = matrix()) {
        
       ##initialise inverse matrix to NULL
        mInv<-NULL
        
        ##used to set the matrix in the list and set inverse to Null as it will be new data
        setMatrix <- function(y) { 
                x<<- y
                mInv<<- NULL
        }
        
        ## function to get the matrix data
        getMatrix<- function() x
        
        ##sets the inverse of the matrix as cached
        setInv<- function(x) mInv<<- x
        
        #gets the stored inverse of the matrix
        getInv<- function() mInv
        
        list(setMatrix=setMatrix, getMatrix= getMatrix, setInv=setInv, getInv=getInv)

}


## CacheSolve: function that returns the inverse of a matrix "x". If it has been calculted previously
## the function retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
        
        ##get the value in the list. 
        mInv <- x$getInv()
        
        ## if the inverse is already calculated, use the cache values
        if(!is.null(mInv)) {
                message("getting cache inverse matrix")
                return(mInv)
        }
        ##if the matrix has not been calculated previously, get values and 
        ##calculate inverse, and set into CacheMatrix List.
        data <-x$getMatrix()
        mInv <- solve(data)
        x$setInv(mInv)
        
        ##return inverse of matrix
        mInv       
        
        
}
