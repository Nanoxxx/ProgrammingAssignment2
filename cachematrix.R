makeCacheMatrix <- function(myMatrix = matrix()) { 		#myMatrix is the input  
  inverseMatrix<-NULL 									#set var for inverse matrix cache
  
  set<-function(interm){								
    myMatrix<<-interm									#Cache myMatrix
    inverseMatrix<<-NULL
  }
  
  get<-function() myMatrix
  
  setInverseMatrix<-function(interm2) inverseMatrix<<- interm2	#Cache inverseMatrix
  
  getInverseMatrix<-function() inverseMatrix
  
  list(set=set, get=get,
       setInverseMatrix=setInverseMatrix,
       getInverseMatrix=getInverseMatrix)
}														#End of makeCacheMatrix

cacheSolve <- function(myMatrix=matrix(), ...) {		
  inverseMatrix<-myMatrix$getInverseMatrix()
  if(!is.null(inverseMatrix)){
    print("get cached data...")
    return(inverseMatrix)
  }
  
  else {
    print("Calculate inverse matrix...")
    calcMatrix<-myMatrix$get()
    inverseMatrix<-solve(calcMatrix, ...)
    myMatrix$setInverseMatrix(inverseMatrix)
    inverseMatrix
  }
}													#End of cacheSolve
