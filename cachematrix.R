## the below function creates an object that caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix())     ## defining the arguments
{
  Inv_mat <- NULL                           ## Initializing the inverse matrix to NULL
  set <- function(y)
  {
    x <<- y                               ## assigning the values of matrix in different environment
    Inv_mat <<- NULL                      
  }
  
  get <- function()x                        ## the get function returns the value of the argument matrix
  
  set_inverse <- function(inverse) Inv_mat <<- inverse  ##  value of Inv-mat in parent environment
  get_inverse <- function() Inv_mat         ## when called this function returns the value of the Inv_mat 
  
  list(set = set,                           ## list of functions, we can call the functions
       get = get,                           ##  by using $ symbol as below ($get)
       set_inverse = set_inverse, 
       get_inverse = get_inverse)
}

## The following function computes the inverse of the matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...)
{
  ## return the inverse of matrix x
  Inv_mat <- x$get_inverse()
  if(!is.null(Inv_mat))                    ## if inverse matrix has already been calculated 
  {                                    ## i.e, if present in cache, return the inverse matrix
    message("getting cached data")
    return(Inv_mat)
  }
  matrix1 <- x$get()
  Inv_mat <- solve(matrix1,...)            ## solving the matrix
  x$set_inverse(Inv_mat)                   ## calling the set_inverse function 
  Inv_mat                                  ## return the inverse matrix
}


## TESTING

##a<-matrix(5:8,2,2)
##> b <- makeCacheMatrix(a)
##> cacheSolve(b)
##     [,1] [,2]
##[1,]   -4  3.5
##[2,]    3 -2.5
