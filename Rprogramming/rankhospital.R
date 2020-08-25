function (state, outcome, num = "best") 
{
    outcome_DF <- read.csv("ass3/outcome-of-care-measures.csv", 
        colClasses = "character")
    states <- unique(outcome_DF$State)
    possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
    try(if (!state %in% states) 
        stop("invalid state"))
    try(if (!outcome %in% possible_outcomes) 
        stop("invalid outcome"))
    h <- hash()
    h[["heart attack"]] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    h[["heart failure"]] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    h[["pneumonia"]] <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    col <- h[[outcome]]
    X <- outcome_DF[outcome_DF$State == state, c(col, "Hospital.Name")]
    X <- X[order(as.numeric(X[, col])), ]
    X <- X %>% group_by(as.numeric(X[, col])) %>% arrange(Hospital.Name, 
        .by_group = TRUE)
    X <- X[complete.cases(X), ]
    if (num == "best") {
        X[1, "Hospital.Name"]
    }
    else if (num == "worst") {
        X[dim(X)[1], "Hospital.Name"]
    }
    else {
        X[num, "Hospital.Name"]
    }
}
