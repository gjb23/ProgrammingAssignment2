## Put comments here that give an overall description of what your
## functions do
##the first function - makeCacheMatrix - takes an invertible matrix as an argument in a generic function
##then enables it to cache its own inverse.
##the second function - cacheSolve - calculates the inverse of that matrix unless it has already been calculated and is in cached data
##in which case it retrieves the already-calculated inverse.

##defines arguments for setting and getting the inverse, using a square matrix x

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  ##internally set x and inv with <<-
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##replaces inv with the inverse of the matrix x unless it has already been calculated in the cache in which case it retrieves that.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  x$setinverse(inv)
  inv
}

##found a test function online to help check work
test = function(mat){
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}
##testing with randomly generated square matrix so it is invertible
set.seed(1110201)
r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)
test(mat1)
