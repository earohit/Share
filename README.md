rankall <- function(outcome, num = "best") {
	
	## Read outcome data
	care <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	## Check that outcome are valid
	if (outcome == "heart attack") {
   		outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	} else if (outcome == "heart failure") {
		outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	} else if (outcome == "pneumonia") {
		outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	}
	else {
		stop("invalid outcome")
	}

	care = care[(care[, outcome]!= "Not Available"), ]
	care[, outcome] <- as.numeric(care[, outcome])
	care <- care[order(care$State, care[ ,outcome], care$Hospital.Name), ]

	MyState <- function(stat) {
		hospName <- NA
		#message(stat)
		new.care <- care[care$State == stat, ]
		if(class(num) == "character") {
			if(num == "best") {
				hospName <- new.care[1, ]$Hospital.Name
			}
			else if(num == "worst") {
				minIndex = which.max(new.care[ ,outcome])
				hospName <- new.care[minIndex, ]$Hospital.Name
			}	
		}
		else {
			if(nrow(new.care) >= num) {
				hospName <- new.care[num, ]$Hospital.Name
			}
		}
		hospName
	}
	
	## For each state, find the hospital of the given rank
	res <- mapply(MyState, unique(care$State))
	d = data.frame(hospital = res, state = unique(care$State))
	d
}

rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	care <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	## Check that state is valid
	if(any(care$State == state) != TRUE) {
		stop("invalid state")
	}

	## Check that outcome is valid
	if (outcome == "heart attack") {
   		outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	} else if (outcome == "heart failure") {
		outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	} else if (outcome == "pneumonia") {
		outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	}
	else {
		stop("invalid outcome")
	}

	care = care[(care$State == state & care[, outcome]!= "Not Available"), ]
	#care[, outcome] <- suppressWarnings(as.numeric(care[, outcome]))
	care[, outcome] <- as.numeric(care[, outcome])
	care <- care[order(care[ ,outcome], care$Hospital.Name), ]

	## Return hospital name in that state with the given rank
	nRows = nrow(care)
	hospName <- NA
	if(class(num) == "character") {
		if(num == "best") {
			hospName <- care[1, ]$Hospital.Name
		}
		else if(num == "worst") {
			minIndex = which.max(care[ ,outcome])
			hospName <- care[minIndex, ]$Hospital.Name
		}	
	}
	else {
		if(nRows >= num) {
			hospName <- care[num, ]$Hospital.Name
		}
	}
	
	hospName
}


makeCacheMatrix <- function(x = matrix()) {
        Matrix <- NULL
        setMatrix <- function(y) {
                x <<- y
                Matrix <<- NULL
        }
        getMatrix <- function() x
        setInvMatrix <- function(InvMatrix) Matrix <<- InvMatrix
        getInvMatrix <- function() Matrix
        list(setMatrix = setMatrix,
		 getMatrix = getMatrix, 
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}

cacheSolve <- function(x, ...) {
        InvMatrix <- x$getInvMatrix()
        if(!is.null(InvMatrix )) {
                message("getting cached data")
                return(InvMatrix)
        }
        Matrix <- x$getMatrix()
        InvMatrix  <- solve(Matrix)
        x$setInvMatrix(InvMatrix)
        InvMatrix
}
