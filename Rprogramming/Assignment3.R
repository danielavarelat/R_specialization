setwd("C:\\Users\\danie\\Documents\\data_science_spe")

library(hash)
library(dplyr)
options(warn=-1)
outcome <- read.csv("ass3/outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])



names(df)[3] <- "attack"
names(df)[4] <- "failure"
names(df)[5] <- "pnem"
df$attack <- as.numeric(df$attack)
df$failure <- as.numeric(df$failure)
df$pnem <- as.numeric(df$pnem)

df <- select(df, c('Hospital.Name',
                        'State',
                        "attack"))

xx<- df %>% group_by(State, attack)  %>% arrange(attack, Hospital.Name, .by_group = TRUE)
xx<-xx[complete.cases(xx),]
data.frame(tapply(xx$Hospital.Name, xx$State, function(x) {x[length(x)]}))


df_out<- data.frame(tapply(xx$Hospital.Name, xx$State, function(x) {}))
names(df_out)[1]<-'hospital'
df_out[2]<-rownames(df_out)
names(df_out)[2]<-'state'


rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome_DF <- read.csv("ass3/outcome-of-care-measures.csv", colClasses = "character")
  outcome_DF <- select(outcome_DF, c('Hospital.Name',
                          'State',
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
  ## Check that state and outcome are valid
  try(if(!outcome %in% possible_outcomes) stop("invalid outcome"))
  ##Map inputs
  h <- hash() 
  h[["heart attack"]]<-"attack"
  h[["heart failure"]]<-"failure"
  h[["pneumonia"]]<-"pnem"
  col<-h[[outcome]]
  df <- select(outcome_DF, c('Hospital.Name',
                     'State', col))
  if (col =="attack"){xx<- df %>% group_by(State, attack)  %>% arrange(attack, Hospital.Name, .by_group = TRUE)}
  if (col =="failure"){xx<- df %>% group_by(State, failure)  %>% arrange(failure, Hospital.Name, .by_group = TRUE)}
  if (col =="pnem"){xx<- df %>% group_by(State, pnem)  %>% arrange(pnem, Hospital.Name, .by_group = TRUE)}
  xx<-xx[complete.cases(xx),]
  ## Return a data frame with the hospital names and the state
  if (num=="best"){df_out<- data.frame(tapply(xx$Hospital.Name, xx$State, function(x) {x[1]}))} 
  else if (num=='worst'){df_out<- data.frame(tapply(xx$Hospital.Name, xx$State, function(x) {x[length(x)]}))} 
  else {df_out<- data.frame(tapply(xx$Hospital.Name, xx$State, function(x) {x[num]}))}
  names(df_out)[1]<-'hospital'
  df_out[2]<-rownames(df_out)
  names(df_out)[2]<-'state'
  df_out
}
dput(rankall, 'rankall.R')

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)


rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_DF <- read.csv("ass3/outcome-of-care-measures.csv", colClasses = "character")
  states <- unique(outcome_DF$State)
  possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
  ## Check that state and outcome are valid
  try(if(!state %in% states) stop("invalid state"))
  try(if(!outcome %in% possible_outcomes) stop("invalid outcome"))
  ## Return hospital name in that state with the given rank
  h <- hash() 
  h[["heart attack"]]<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  h[["heart failure"]]<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  h[["pneumonia"]]<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  col<-h[[outcome]]
  X<-outcome_DF[outcome_DF$State==state,c(col,'Hospital.Name')]
  X <- X[order(as.numeric(X[,col])),]
  X<-X %>%
    group_by(as.numeric(X[,col]))  %>%
    arrange(Hospital.Name, .by_group = TRUE)
  X<-X[complete.cases(X), ]
  
  if (num=="best"){X[1,'Hospital.Name'] } 
  else if (num=='worst'){ X[dim(X)[1],'Hospital.Name']} 
  else {X[num,'Hospital.Name']}
  
}

rankhospital("MN", "heart attack", 5000)

dput(rankhospital, 'rankhospital.R')

best <- function(state, outcome) {
  ## Read outcome data
  outcome_DF <- read.csv("ass3/outcome-of-care-measures.csv", colClasses = "character")
  states <- unique(outcome_DF$State)
  possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  try(if(!state %in% states) stop("invalid state"))
  try(if(!outcome %in% possible_outcomes) stop("invalid outcome"))
  
  ##Match outcome with real column name
  h <- hash() 
  h[["heart attack"]]<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  h[["heart failure"]]<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  h[["pneumonia"]]<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  col<-h[[outcome]]
  ## Return hospital name in that state with lowest 30-day death
  X<-outcome_DF[outcome_DF$State==state,c(col,'Hospital.Name')]
  ind<- which(X==min(as.numeric(X[,col]), na.rm=T))
  if (length(ind)>1){
    names <- X$Hospital.Name[ind]
    hosp<-sort(names)[1]
  } else(hosp<- X$Hospital.Name[ind])
  
  hosp
  ## rate
}
best("TX", "heart failure")

