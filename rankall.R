## Function to rank hospitals in all states based on a specified
## outcome.

rankall <- function(outcome, num = "best"){
    
    ## Read the data
    outcomeData <- read.csv("outcome-of-care-measures.csv",
                            colClasses = "character")
    
    ## validate outcome 
    if (!(outcome %in% c("heart attack", "heart failure", 
                         "pneumonia"))){
        stop("Invalid outcome")
    }
    
    # leanData <- as.data.frame(cbind(outcomeData[,2],
    #                                 outcomeData[,7],
    #                                 outcomeData[,11],
    #                                 outcomeData[,17],
    #                                 outcomeData[,23]),
    #                           stringsAsFactors = FALSE)
    
    ## Get the states levels
    states <- levels(factor(outcomeData[,7]))
    
    ## Based on outcome, set the column number to rank on
    col2Rank <- if(outcome == "heart attack") {11}
        else if (outcome == "heart failure") {17}
        else {23}
    
    ## ensure the ranking is based on numeric sorting
    outcomeData[,col2Rank] <- as.numeric(
        outcomeData[,col2Rank])
    
    ## sort the entire data on 3 factors - 1st the state;
    ## second - the outcome value; third - the hospital name
    
    sortedData <- outcomeData[order(outcomeData[,7],
                                    outcomeData[,col2Rank],
                                    outcomeData[,2],
                                    na.last = NA),]
    sortedData <- sortedData[!is.na(sortedData[,col2Rank]),]
    
    ## split sorted data into a list of data frames based on state
    ## Get the specific ranking from each state using split
    ## and lapply
    listStates <- split(sortedData[,2],
                        sortedData[,7])

    requestRank <- function(x, num){
        if(num == "best"){
            head(x,1)
        } else if(num == "worst"){
            tail(x,1)
        } else{
            x[num]
        }
    }

    result <- lapply(listStates, requestRank, num)
    ##result
    ##listStates
    ##sortedData
    
    ## format the result
    
    output <- data.frame(hospital = unlist(result),
               state = names(result),
               row.names = names(result))
    output
    
}