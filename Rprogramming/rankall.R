function (outcome, num = "best") 
{
    outcome_DF <- read.csv("ass3/outcome-of-care-measures.csv", 
        colClasses = "character")
    outcome_DF <- select(outcome_DF, c("Hospital.Name", "State", 
        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
        "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
    names(outcome_DF)[3] <- "attack"
    names(outcome_DF)[4] <- "failure"
    names(outcome_DF)[5] <- "pnem"
    outcome_DF$attack <- as.numeric(outcome_DF$attack)
    outcome_DF$failure <- as.numeric(outcome_DF$failure)
    outcome_DF$pnem <- as.numeric(outcome_DF$pnem)
    states <- unique(outcome_DF$State)
    possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
    try(if (!outcome %in% possible_outcomes) 
        stop("invalid outcome"))
    h <- hash()
    h[["heart attack"]] <- "attack"
    h[["heart failure"]] <- "failure"
    h[["pneumonia"]] <- "pnem"
    col <- h[[outcome]]
    df <- select(outcome_DF, c("Hospital.Name", "State", col))
    if (col == "attack") {
        xx <- df %>% group_by(State, attack) %>% arrange(attack, 
            Hospital.Name, .by_group = TRUE)
    }
    if (col == "failure") {
        xx <- df %>% group_by(State, failure) %>% arrange(failure, 
            Hospital.Name, .by_group = TRUE)
    }
    if (col == "pnem") {
        xx <- df %>% group_by(State, pnem) %>% arrange(pnem, 
            Hospital.Name, .by_group = TRUE)
    }
    xx <- xx[complete.cases(xx), ]
    if (num == "best") {
        df_out <- data.frame(tapply(xx$Hospital.Name, xx$State, 
            function(x) {
                x[1]
            }))
    }
    else if (num == "worst") {
        df_out <- data.frame(tapply(xx$Hospital.Name, xx$State, 
            function(x) {
                x[length(x)]
            }))
    }
    else {
        df_out <- data.frame(tapply(xx$Hospital.Name, xx$State, 
            function(x) {
                x[num]
            }))
    }
    names(df_out)[1] <- "hospital"
    df_out[2] <- rownames(df_out)
    names(df_out)[2] <- "state"
    df_out
}
