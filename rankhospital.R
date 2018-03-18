## Function to rank hospitals in a specified state on a specified
## outcome.

rankhospital <- function(state, outcome, num = "best"){
    ## Read the data
    outcomeData <- read.csv("outcome-of-care-measures.csv",
                            colClasses = "character")
    
    ## Check validity of inputs
    outcomeTypes <- c("heart attack", "heart failure", 
                      "pneumonia")
    
    if(! state %in% outcomeData[,7]){
        stop("Error: state is invalid")
    }
    
    if(! outcome %in% outcomeTypes) {
        stop("Error: outcome is invalid")
    }
    
    ## Extract state specific data
    stateData <- outcomeData[outcomeData$State == state,]
    
    ## Check if num is higher than number of hospitals in state
    if (is.numeric(num) && num > nrow(stateData)){
        message(cat("Not that many hospitals in ", state))
        return(NA)  
    }
    
    ## Extract just the data required
    if (outcome == "heart attack"){
        column <- 11
    } else if(outcome == "heart failure") {
        column <- 17
    } else {
        column <- 23
    }
    
    # hospnames <- stateData[!is.na(stateData[,23]),2]
    # rate <- stateData[as.numeric(stateData[,23]),23]
    stateData[, column] <- as.numeric(stateData[,column])
    stateDataSorted <- stateData[order(stateData[,column],
                                       stateData[,2]),]
    stateDataSorted <- stateDataSorted[(!is.na(stateDataSorted[,column])),]
    
    
    # keydata <- as.data.frame(cbind(hospnames, as.numeric(rate),
    #                          stringsAsFactors = FALSE))
    # colnames(keydata) <- c("Hospital", "Rate")
    # 
    # keydatasorted <- keydata[order(keydata[,"Rate"],
    #                                keydata[,"Hospital"]),]
    if(num == "best") num = 1
    if(num == "worst"){
        num <- nrow(stateDataSorted)
    }
    
    stateDataSorted[num,c(2,column)]
}