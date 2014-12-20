## Coursera - R Programming, programming assigment 2
## The cache matrix

## makeCacheMatrix: creates an object of type cahcedMatrix
## Initialitates the inverted matrix to NULL
## creates the get and set fucntions to get and set the original matrix x
## creates the get and set invertedMatrix methods to get and set the inverted version of x


makeCacheMatrix <- function(x = matrix()) {
      inverted_matrix<-NULL
 
      getInverted<-function() inverted_matrix
      setInverted<-function(im)inverted_matrix<-im
      
      get<-function() x
      set<-function(y){
        inverted_matrix<<-NULL
        x<<-y
      }
      list(set=set,get=get,setInverted=setInverted,getInverted=getInverted)
}


## cacheSolve: given am mtrix x, this function returns its inverted, if there was a previous
## calculation of the inverted marix, the inversion is not calculated. On the other hand if the 
## inverted matrix of x was never caculated before it is calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getInverted()
        if(!is.null(m)){
          ## Return the pre-calculated inverted matrix
          return(m)
        }
        mdata<-x$get()
        ##Solve the inverse of mdata
        m<-solve(mdata)
        x$setInverted(m)
        m
}
