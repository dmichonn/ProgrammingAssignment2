## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        a<-NULL
        set<-function(b){
                x<<-b
                a<<-NULL
        }
        get<-function() x
        setInverse_matrix<-function(solve) a<<-solve
        getInverse_matrix<-function() a
        list(set=set, get=get, 
	setInverse_matrix=setInverse_matrix, 
	getInverse_matrix=getInverse_matrix)
}

## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        a<-x$getInverse_matrix()
        if(!is.null(a)){
                message("getting cached data")
                return(a)
        }
        data<- x$get()
        a<-solve(data,...)
        x$setInverse_matrix(a)
        a
        ## Return a matrix that is the inverse of 'x'
}
