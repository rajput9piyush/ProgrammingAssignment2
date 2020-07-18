## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will
## not discuss here). Your assignment is to write a pair of functions that
## cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv_matrix = NULL
      set <- function(y){
            x <<- y
            inv_matrix <<- NULL
      }
      get <- function() x
      setinv <- function(inv_mat) inv_matrix <<- inv_mat
      getinv <- function() inv_matrix
      
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
      ## Return a matrix that is the inverse of 'x'
      inv_matrix <- x$getinv()
      if(!is.null(inv_matrix)){
            message("getting Cached Inverse Matrix")
            return(inv_matrix)
      }
      temp <- x$get()
      inv_matrix <- solve(temp)
      x$setinv(inv_matrix)
      inv_matrix
}