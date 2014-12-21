## Comprises of a pair of functions which cache the inverse of
## a matrix, as computing the inverse is a costly computation

## SAMPLE EXAMPLE CODE FROM COURSERA
# makeVector <- function(x = numeric()) {
#       m <- NULL
#       set <- function(y) {
#             x <<- y
#             m <<- NULL
#       }
#       get <- function() x
#       setmean <- function(mean) m <<- mean
#       getmean <- function() m
#       list(set = set, get = get,
#            setmean = setmean,
#            getmean = getmean)
# }

## This function creates a special "matrix" object that can cache its inverse.
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


## SAMPLE EXAMPLE CODE FROM COURSERA
# cachemean <- function(x, ...) {
#       m <- x$getmean()
#       if(!is.null(m)) {
#             message("getting cached data")
#             return(m)
#       }
#       data <- x$get()
#       m <- mean(data, ...)
#       x$setmean(m)
#       m
# }

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
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
