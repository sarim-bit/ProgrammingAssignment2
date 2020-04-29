## This function as a whole computes and stores the inverse of a matrix,so that if we have to compute the inverse of a
## matrix which was aleady calculated it can simply recall it from memory

## makeCacheMatrix takes a matrix as its input and using the CacheSolve function stores its inverse or returns it if
## it was already stored. It basically creates a special Matrix with function to set and get the values of 
## a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
                   s<-NULL
                   set<-function(y){
                        x <<- y
                        s <<- NULL
                   }
                   get<-function()x
                   set_inv <- function(inv) s <<- inv
                   get_inv <- function() s
                   list(set=set,get=get,set_inv=set_inv,get_inv=get_inv)
          
  }


## The following function calculates the inverse of the special matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse
## of the data and sets the value of the inverse in the cache via the set_inv function.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$get_inv()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$set_inv(s)
  s
}
