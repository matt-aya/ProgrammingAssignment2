

## The following are two functions:

## They cache and compute the inverse of a matrix.


                ## FIRST : Function one:


## 1. Creates a special "matrix" object
## 2. compute it's inverse with --- solve() ---
## 3. cache the result into getinv


makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  set <- function(x) {
    
    # assign variable mat to the matrix
    # assign Empty variable to inverse
    mat <<- x;
    inverse <<- NULL;
  }
  
  get <- function() mat;
  
  # Very Important :
  # use --- solve() --- to calulate the inverse
  
  setinv <- function(inv) inverse <<- solve(mat);
  
  # now cache the inverse into getinv
  
  getinv <- function() inverse;
  
  # Assignthe variables
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


                ## SECOND : Function two:


## computes the inverse of the special "matrix" returned by
## the previous function makeCacheMatrix .
## If the inverse has already
## been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(mat, ...) {
  
  # 1. check if the inverse has been calculated
  
  inverse <- mat$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  
  # 2. else retrieve the inverse from the cache use ---solve()---
  
  data <- mat$get()
  invserse <- solve(data)
  mat$setinv(inverse)
  return(inverse)
}



                ## Now time to try it all out


## 1. create a Square Invertible matrix

c <- rbind(c(3,5),c(-7,2))

## 2. assign the first function

a <- makeCacheMatrix()

## 3. call set and get and check the variable assignment

a$set(c)
a$get()

## 4. Now the FUN part: inverse and check the result

a$setinv(c)
a$getinv()

## 5. Second function, assign and check the cached result.

e <- cacheSolve(a)
e

## That's was my assignment, hope it works correclty :)
## good luck.




