#---------------------------programming assignment -2 -- Hatlock ------------------------

####################################################################################################################
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix # 
# rather than compute it repeatedly.The following following two functions are used to cache the inverse of a matrix.#
# makeCacheMatrix function creates a special "matrix" object that is able to cache its inverse.                     #
# cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.               #
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should           #
# retrieve the inverse from the cache.                                                                              #
####################################################################################################################

#######################################################################################################
#This special matrix object **makeCacheMatrix** is  a list that contains:       
# set the value of the matrix                                                   
# get the value of the matrix                                                   
# set the value of the inverse (inv)                                            
# get the value of the inverse(inv)                                             
#######################################################################################################

makeCacheMatrix  <- function(x= matrix()){   # input x will be a matrix
        
        # inv stores the inverse value and also resets to NULL everytime
        # makeCacheMatrix is called.
        inv <- NULL  
        
        # sets a new matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # returns the value of the original matrix when called by cacheSolve()
        
        get <- function() {x}  
        
        # this is called by cacheSolve() during the first cacheSolve()  
        # run and it will store the value using super-assignment
        
        setinv <- function(solve)  
        {inv <<- solve}    #
        
        #returns the cached value when called by cacheSolve()
        
        getinv <- function() {inv} 
        
        
        #list returned with newly created objcts
        list(   set = set,         
                get = get,        
                setinv = setinv,   
                getinv = getinv)  
        
}

################################################################################################
# cacheSolve function returns the inverse of the matrix. Before it computes the inverse,
#it checks whether it has been computed and stored(chached). It yes, then it accesses and returns
#the cached value. If no, it computes the inverse and caches the value by invoking the setinverse
#function from makeCacheMatrix()
#################################################################################################

cacheSolve <- function(x, ...){    # the input 'x' is an object created by makeCacheMatrix
        
        #access the object "x" and get the value inv
        inv <- x$getinv()          
        
        # if inv has already been cached (not NULL), then send this message to console abd return the inverse value
        
        if(!is.null(inv)) {        
                message("getting cached data")  
                return(inv)                    
        }
        
        # if inv is not found in the cache, we invoke to access the object 'x' and return the matrix
        data <- x$get()            
        
        #then calculate the inverse of the matrix suing the solve() function
        inv <- solve(data, ...)    
        
        #then store the calculated inverse value in x (by invoking the setinv() function of object 'x' in makeCacheMatrix
        x$setinv(inv) 
        
        #and then return the inverse value(inv)
        
        inv                        
}

#####################
#~~Sample run~~ 
#####################


# a  <- matrix (1:4,2,2)    
# > x <- makeCacheMatrix(a) 
# > x$get()                 
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(x)           
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(x)           
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5


