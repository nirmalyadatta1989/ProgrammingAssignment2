##WEEK 3 Assignment Coursera Data Science R Programming Course - nirmalyadatta1989

## This function is creating a special "matrix" object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ##define argument with default matrix mode
  inv <- NULL                               ##initializing inv as null and will holdvalue of matrix inverse in future
  set <- function(y) {
    x <<- y                                 ##value of matrix in upper environment that is the parent environment
    inv <<- NULL
  }
  get <- function() x                       ##returns value of matrix argument
  setinverse <- function(inverse) inv <<- inverse ##assigning inv in parent environment
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function returns an inverse of the matrix by computing inverse of the special "matrix" returned by "makeCacheMatrix" above
## It checks if the inverse has been computed earlier for the matrix
## If yes, it fetches from the cache and does not re compute provided the matrix has not changed
## If not or if the matrix has changed, it computes the inverse and then sets the value in the cache using setinverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("GETTING CACHED DATA AS INVERSE OF MATRIX WAS CALCULATED EARLIER AND IS PRESENT IN CACHE")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
