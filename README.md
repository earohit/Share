best <- function(state, outcome) {
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

	care = care[care$State == state,]
	care[, outcome] <- suppressWarnings(as.numeric(care[, outcome]))

	## Return hospital name in that state with lowest 30-day death
	m = min(care[, outcome], na.rm = TRUE)
	index <- which(care[, outcome] == m)
	#message(length(index))
	rank <- sort(care[index, ]$Hospital.Name)
	rank[1]
}

#best("TX", "heart attack")
