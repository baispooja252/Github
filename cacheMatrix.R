## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function Function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
#can cache its own object. 

#<<- operator is used to assign a value to an object in an environment that is different 
#from the current environment 

makeCacheMatrix <- function(x = matrix()) {
 
    inv <- NULL
    #set the value of the Matrix
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function() x                                          #get the value of the Matrix
    setInverse <- function(solveMatrix) inv <<- solveMatrix     #set the value of Inverse Matrix
    getInverse <- function() inv                                #get the inverse Matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)


}


## Write a short comment describing this function
## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
# and set the invertible  matrix by using the solve function.
# In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
#after running the code 1st time), it returns a message  "Getting Cached Invertible Matrix" 
#and the cached object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #get the value of the invertible matrix from the makeCacheMatrix function
  inv <- x$getInverse()
  if(!is.null(inv)){                         #if inverse matrix is not NULL
    message("getting cached data")           #Type message: Getting Cached Invertible Matrix 
    return(inv)                              #return the invertible matrix
  }
  
  #if value of the invertible matrix is NULL then 
  data <- x$get()                            #get the original Matrix Data 
  inv <- solve(data)                         #use solve function to inverse the matrix
  x$setInverse(inv)                         #set the invertible matrix 
  return(inv)                                       #return the invertible matrix
}


#########Testing##########

####Test 1 [2*2 Matrix] #####
TestMatrix <- matrix(1:4,2,2)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$get()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)

