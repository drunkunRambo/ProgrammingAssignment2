## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<- function(y){
    x<<-y
   inv<<- NULL
  }
  get<- function() x
  getInv<-function() inv
  setInv<- function(iv)inv<<-iv
  list(set=set,get=get,getInv=getInv,setInv=setInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setInv(inv)
  inv
}

mat1<-matrix(c(10,23,34,56),nrow = 2)
amatrix <- makeCacheMatrix(mat1)
amatrix$get()
amatrix$getInv()
amatrix$set(mat1)
cacheSolve(amatrix)
amatrix$getInv()





