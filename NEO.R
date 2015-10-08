#lexical scoping exercise: function creates a matrix object
#that can cache its inverse using gaussian elimination
cacheMatrix = function(neo = c()) {
    
    # function to be used to determine if the length of a 
    # vector is compatible with a square matrix
    is.whole = function(a) { 
        (is.numeric(a) && floor(a) == a) ||
        (is.complex(a) && floor(Re(a)) == Re(a) && floor(Im(a)) == Im(a))
    }
    
    sqnxm = sqrt(length(neo))
    if(!all(is.whole(sqnxm))) {
        neo = ("Morpheus is beginning to doubt your competency 
                with the matrix function; please revise your vector elements
                for a square matrix before Morpheus force-feeds you the 
                blue pill.")
        } else {
            neo = matrix(neo, nrow = sqnxm, ncol = sqnxm)
            smith = NULL
        
            set = function(trin) {
                neo <<- trin
                smith <<- NULL
        }
    
        get = function() neo 
            setInverse = function(solve) smith <<- solve
            getInverse = function() smith
            list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
    }
    
}
  
#Function returns the inverse of the cached squared matrix   
cacheInverse = function(neo, ...) {
    
    if(all(!is.list(neo))) {
        cat(strwrap(neo))
        } else {
            smith = neo$getInverse()
            data = neo$get()
            smith = solve(data, ...)
            neo$setInverse(smith)
            smith
        
        }
       
}


