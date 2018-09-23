## With this functions i'm trying to calculate the inv. of a matrix
##(with the assumption that the matrix invertible)
##this kind of elements (the inv of a matrix) are particulary important to 
## do the "math" with data sets. It's easy when you have small data groups (small matrice)
## but when we have lots of data, it is better to make the calculation with matrices.
## For example, in an multivariate regression models. 


##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# makeCacheMatrix operates as follows:
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix


## This first function will take the matrix "x" (that is the matrix i create, or i import via a data set)
makeCacheMatrix <- function(x = matrix()) {
#first of all we will create the "space" inv, which will be empty until we put a value in it
inv <- NULL
## now we will set the value "y" to the matrix "x" 
set <- function(y) { 
  x <<- y 
  inv <<- NULL
}
## after that we will get the value of "x"
get <- function() x
## follow by the same procedure, but now for the inv value of the matrix.
setinverse <- function(inverse) 
inv <<- inverse

getinverse <- function() inv

list(set=set, 
     get=get, 
     setinverse=setinverse, 
     getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cacheSolve should retrieve the inverse from the cache.


#For this, we will take the same x matrix that was use for the makeCacheMatrix function.
## this new function will calculate the Inv. of the x matrix, or, if the value has been calculated prevously,
## will extract it from the memory (cache)
cacheSolve <- function(x, ...) {
 inv <- x$getinverse()
if(!is.null(inv)) { message("getting cached data.")
  return(inv)}
 
 
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
       
}
## Return a matrix that is the inverse of 'x'

#Testing my functions with a example of a matriz 5x5
m <- matrix(nrow=5,ncol=5, c(1,4,6,9,3,2,1,6,9,6,7,7,4,9,5,6,1,2,3,4,5,6,7,8,9))
mk <- makeCacheMatrix(m)
cs <- cacheSolve(mk)
print(m)
print(cs)
