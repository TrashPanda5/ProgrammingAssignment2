## These two functions work together to solve for the inverse of a matrix
## and save previously solved matrices in Cache. makeCacheMatrix is a function that caches 
## the inverse of a matrix and defines multiple elements, while cacheSolve first checks 
## if an inverse of a matrix is saved in Cache, then solving for the inverse if the inverse 
## is not saved in cache. To utilize the two functions you define a matrix vector 
## using makeCacheMatrix (test <- makeCacheMatrix(matrix1). This maintains the elements defined 
## in makeCacheMatrix as part of the primary programming environment and allows them to be 
## accessed by cacheSolve. A vector used in cacheSolve must be created using makeCacheMatrix 
## because cacheSolve relies on functions defined in makeCacheMatrix. 

## makeCacheMatrix first uses the set function to clear any previously stored matrices by 
## using the <<- object. This assigns the value to the element in the parent environment too. 
## The next three elements define functions that are used to access the original matrix (get), 
## store the inverse matrix (setmatrix), and retrieve the inverse stored matrix (getmatrix). 
## Last, list names each function so they can be called on from the programing environment and used 
## in cacheSolve. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)}


## cacheSolve first checks to see if the inverse of a matrix is stored in cache. x$getmatrix
## calls on the function defined in makeCachematrix to see if the inverse matrix is in cache. 
## if this is saved, it returns the matrix 'm' along with the message "getting cached data". 
## If the element was not saved in cache, it uses x$get() to get the data, and solves for the inverse
## (solve(data,...)) it then stores the inverse matrix using the function setmatrix defined in 
## makeCacheMatrix.  (x$setmatrix(m))

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
  
  }

## This is an example matrix. 
exampmatrix <- matrix(c(1, -1, -3, 4), nrow = 2, ncol = 2)
## this is an example of using the makeCacheMatrix function to create an object with 
## the exampmatrix data and the functions defined in makeCacheMatrix
test <- makeCacheMatrix(exampmatrix)
# this returns the inverse of exampmatrix and stores in cache. 
cacheSolve(test)
# if run immediately after cacheSolve(test) this will return the cached matrix and a message that
#the data was from cache. 
cacheSolve(test)
