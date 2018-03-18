## This function is expected to identify the best hospital
## in a given state for the specified 'outcome'
## Outcomes are limited to - heart attack, heart failure
## and pneumonia
## The best hospital in the state is the one with the 
## lowest mortality rate for the specific case

best <- function(state, outcome){
    ## Read the data
    outcomeData <- read.csv("outcome-of-care-measures.csv",
                        colClasses = "character")
    
    ## Check validity of inputs
    outcomeTypes <- c("heart attack", "heart failure", 
                      "pneumonia")

    ## Tried any(state,outcomeData[,7], na.rm = TRUE))
    if(! state %in% outcomeData[,7]){
        stop("Error: state is invalid")
    }
    ## Tried any(outcome, outcomeTypes, na.rm = TRUE))
    if(! outcome %in% outcomeTypes) {
        stop("Error: outcome is invalid")
    }
    ## Identify the best in the state
    
    # extract state data
    stateData <- outcomeData[outcomeData$State == state,]
    min11 <- min(as.numeric(stateData[,11]), na.rm = TRUE)
    min17 <- min(as.numeric(stateData[,17]), na.rm = TRUE)
    min23 <- min(as.numeric(stateData[,23]), na.rm = TRUE)
    
    if (outcome == "heart attack"){
        hosp <- stateData[stateData[,11] == min11,2]
    } else if(state == "heart failure") {
        hosp <- stateData[stateData[,17] == min17,2]
    } else {
        hosp <- stateData[stateData[,23] == min23,2]
    }
    
    if(length(hosp) > 1){
        hosp <- sort(hosp)
    }
    hosp[1]
    
}