## The first function creates additional objects that it associates with an input matrix, M; these are cached.
## The second function checks to see if the inverse for an intertible matrix already exists in memory, uses the cache if the inverse is already cached (thereby saving time by not re-computing), otherwise it will compute the inverse required. The inverse is then returned.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Note: For this assignment we are allowed to assume m is invertible, if this were not the case, then test per commented out lines here
  ## Checks M is square, and invertible -- returns error messages and breaks out of function if M fails requirements
  ## if(length(dim(M))!=2){print('variable is not a 2x2 matrix'); stop()}
  ## if(dim(M)[1]!=dim(M[2])){print('Matrix entered must be square'); stop()}
  ## if(det(M)==0){print('Matrix is square, but is not invertible.'); stop()}

makeCacheMatrix <- function(M = matrix()){
  ## Takes matrix M
  ## For this assignment we are allowed to assume m is invertible, if this were not the case, then test per commented out lines here
  ## Checks M is square, and invertible -- returns error messages and breaks out of function if M fails requirements
  ## if(length(dim(M))!=2){print('variable is not a 2x2 matrix'); stop()}
  ## if(dim(M)[1]!=dim(M[2])){print('Matrix entered must be square'); stop()}
  ## if(det(M)==0){print('Matrix is square, but is not invertible.'); stop()}

  invM <- NULL
  ## use `<<-` operator to assign a value to an object in an environment that is different from the current environment.
  set <- function(y){
    M <<- y
    invM <<- NULL
  }
  get <- function() M
  setinv <- function(inverse) invM <<- inverse
  getinv <- function() invM
  ## Return a list containing functions that
  ##   1. set the value of the matrix
  ##   2. get the value of the matrix
  ##   3. set the value of the inverse
  ##   4. get the value of the inverse
  ## This list is then used by cacheSolve()
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes the List output from makeCacheMatrix.
## If the required inverse has already been calculated and cached, then cacheSolve retrieves the inverse from the cache.
## If the required inverse is not in cache, then cacheSolve will compute the required inverse, and cache it. 
## If we can assume the original matrix is called myMatrix, and then execute: 
##   temp = makeCacheMatrix(myMatrix) 
##   cacheSolve(temp)
## to get the inverse from cache if possible, otherwise recompute.
## Given myMatrix has global scoping, we can test inside cacheSolve to see if the original matrix has changed after using makeCacheMatrix (and thus, that any cached inverse would no longer be valid), by
## nesting the entire if statement with a second if statement
## if(myMatrix!=x$get()){
##      message("The orignal matrix has changed, computing the new inverse")
##      myInv <- solve(myMatrix)}
## else{DO THE ORIGINAL IF STATEMENT AS IN cacheSolve BELOW}

cacheSolve <- function(x, ...) {
  myInv <- x$getinv()
  if(!is.null(myInv)) {
    message("Getting cached data")
    return(myInv)
  }
  else{ # calculate the inverse if it's not in the cache
    myMatrixData <- x$get()
    myInv <- solve(myMatrixData)
    x$setinv(myInv) # after calculating the Inverse here, it also caches a copy using the setinv function from makeCacheMatrix
  }
  myInv ## implicit return of the value of myInv (the inverse of the original matrix M)
}
