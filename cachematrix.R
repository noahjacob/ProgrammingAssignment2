## Put comments here that give an overall description of what your
## functions do

## This function makes the matrix object with the cached variables to store the 
# inverse of the matrix if already computed

makeCacheMatrix <- function(x = matrix()) {
  #cacheinv stores the inverse if computed initially is NULL
  cacheinv <- NULL
  
  #sets the matrix
  set<- function(y){
    x<<-y
    cacheinv<<-NULL
  }
  
  #get the matrix
  get <- function() x
  
  #sets the inverse
  setinv <- function(inv) cacheinv <<- inv
  
  #gets the cached inverse value
  getinv <- function() cacheinv
  list(set = set, get = get,
       setinv=setinv, getinv = getinv)
}


## This function computes the inverse of the set matrix. It checks if the inverse
##has already been computed if not it computes it and stores it as cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv() #gets the current value of cached inverse.
  
  #checks if there's a computed inverse and returns the value if it is already computed.
  if(!is.null(inv)){
    message("getting cached inverse of the matrix")
    return(inv)
  }
  #if cachedinv is empty, the inverse is computed and stored using setinv function. 
  mat <- x$get()
  inv <- solve(x)
  x$setinv(inv)
}



## Output in console

# > mat<-matrix(c(1,4,9,0,-3,2,2,7,8),3,3)
# > m1 <- makeCacheMatrix(mat)
# > cacheSolve(m1)
# [,1]    [,2]     [,3]
# [1,] -1.18750  0.1250  0.18750
# [2,]  0.96875 -0.3125  0.03125
# [3,]  1.09375 -0.0625 -0.09375
# > cacheSolve(m1)
# getting cached data
# [,1]    [,2]     [,3]
# [1,] -1.18750  0.1250  0.18750
# [2,]  0.96875 -0.3125  0.03125
# [3,]  1.09375 -0.0625 -0.09375

