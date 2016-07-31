## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first fucntion just creates a list that enables:
## a  set the matrix
## b  get the matrix
## c  set the inverse
## d  get the inverse


makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL 
    set <- function(y) { 
         x <<- y 
         inv <<- NULL 
       } 
    get <- function() x 
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv 
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
   } 


## Write a short comment describing this function
## the function below is the workhorse
## before it tries to solve for the inverse of a matrix, checks the cache whether the list contains the inverse
## if the inverse already exists, then it retrieves the answer from the cache
## else it proceeds to solve for the inverse an returns that.


cacheSolve <- function(x, ...) { 
    inv <- x$getinverse() 
    if(!is.null(inv)) { 
         message("getting cached data.") 
         return(inv) 
     } 
    data <- x$get() 
    inv <- solve(data) 
    x$setinverse(inv) 
    inv 
} 

## testing if it works
## create a matrix x and run the makecachematrix


##> x <- rbind(c(1, -4), c(-4, 1))
##> m <- makeCacheMatrix(x)

## Take a look at the list

##> m
##$set
##function (y) 
##{
##  x <<- y
##  inv <<- NULL
##}
##<environment: 0x0000000002e8e038>
##  
##  $get
##function () 
##  x
##<environment: 0x0000000002e8e038>
##  
##  $setinverse
##function (inverse) 
##  inv <<- inverse
##<environment: 0x0000000002e8e038>
##  
##  $getinverse
##function () 
##  inv
##<environment: 0x0000000002e8e038>
##  

## extract the matrix from the list

##  > m$get() 
##      [,1] [,2]
##[1,]    1   -4
##[2,]   -4    1

## solve for th einverse, 
## since this is th efirst time it is being run, there is nothing in the cache to refer to

##> cacheSolve(m) 
##        [,1]        [,2]
##[1,] -0.06666667 -0.26666667
##[2,] -0.26666667 -0.06666667

## the above is th esolution calculated and kept in cache
## now rerun the function, and this time it will retrieve from the cache rather than solving for th einverse


##> cacheSolve(m) 
##getting cached data.
##[,1]        [,2]
##[1,] -0.06666667 -0.26666667
##[2,] -0.26666667 -0.06666667

## done.