## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inversematrix <<- inverse
  getinv <- function() inversematrix
  list(set = set, get = get,setinverse = setinv,getinverse = getinv)

}


#This function returns the inverse of the matrix created via makeCacheMatrix.
#If the inverse is already present in the cache, it will display that value instead of recalculating.

cacheSolve <- function(x, ...) {
  inversematrix <- x$getinv()
  if(!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  }
  data <- x$get()
  inversematrix <- solve(data)
  x$setinv(inversematrix)
  inversematrix
}

#My Function Testing
#x <- cbind(c(2,4), c(6,88))
#m1 <- makeCacheMatrix(x)
#> m1$get()
#     [,1] [,2]
#[1,]    2    6
#[2,]    4   88
#> cacheSolve(m1)
#            [,1]        [,2]
#[1,]  0.57894737 -0.03947368
#[2,] -0.02631579  0.01315789
#> cacheSolve(m1)
#getting cached data  -- the comment gets printed out when we get the cached value back successfully.
#            [,1]        [,2]
#[1,]  0.57894737 -0.03947368
#[2,] -0.02631579  0.01315789
