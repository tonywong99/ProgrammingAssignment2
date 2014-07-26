## Coursera R programming Assignment 2
## #### Test Run ####
## mmm<-makeCacheMatrix()
## mmm$set(matrix(1:4,2,2))
## cacheSolve(mmm)


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        mat<-NULL
        set<-function(y){
                x<<-y
                mat<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) mat<<- solve
        getmatrix<-function() mat
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. 
cacheSolve <- function(x=matrix(), ...) {
        mat<-x$getmatrix()
        if(!is.null(mat)){
                message("getting cached data")
                return(mat)
        }
        matrix<-x$get()
        mat<-solve(matrix, ...)
        x$setmatrix(mat)
        mat
}















