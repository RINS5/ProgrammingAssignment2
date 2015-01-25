## The makeCacheMatrix function provides control functions for getting and setting a matrix 
## as well as getting and setting the calculated inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	  matrixInverse <- NULL		#set matrixInverse to NULL when initializing makeCacheMatrix()function since no inverse has been calcualted yet
        set <- function(y) {		#The function "set" stores the user's new matrix when called
                x <<- y			#Take the matrix received in 'y' and store it in the function's parent environment variable 'x'
		    matrixInverse <<- NULL #set the function's parent environment variable "matrixInverse" to NULL since a NEW matrix was received and no inverse has been calcualted yet
        }
        get <- function() x		#The function "get" returns the user's matrix
	  setInverse <- function(inverse) matrixInverse <<- inverse #Store the computed inverse matrix receieved as "inverse" in the function's parent environment variable "matrixInverse"
	  getInverse <- function() matrixInverse #The function "getInverse" returns the computed inverse of a matrix
        #Declare the functions as a list 
	  list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## The cacheSolve function will calcualte the inverse of a matrix 
## and set the variables defined in the makeCacheMatrix function

cacheSolve <- function(x, ...) {
	  matrixInverse <- x$getInverse()	#Retrieve the current matrix's computed inverse using the function getInverse()
	  if(!is.null(matrixInverse)) {	#If the current matrix's inverse is not null and therefore has already been calculated then...
                message("getting cached data")	#Display Message and...
                return(matrixInverse)	#Return the old/current inverse matrix that was stored for the last inverse calculation.
        }

	  dataMatrix <- x$get()			#Retrieve the user's matrix and store it in dataMatrix
	  matrixInverse <- solve(dataMatrix) #Calculate the inverse of the matrix stored in "dataMatrix" using the solve() function and store it in matrixInverse
	  x$setInverse(matrixInverse)		#send the new calculated inverse matrix to the setInverse() function for storing
	  matrixInverse				#return the new calculated invers matrix
        ## Return a matrix that is the inverse of 'x'
}
