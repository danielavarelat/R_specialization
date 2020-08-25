 
setwd("C:\\Users\\danie\\Documents\\data_science_spe")


library(dplyr)
library(stringr)

pollutantmean <- function(directory,pollutant, id=1:332){
  if (length(id) ==1){
    file <- file.path(directory, sprintf("%03d.csv", id))
    result <- read.csv(file)
  }
  else {
    files <- list()
    for (i in id){
      files[i] <- file.path(directory, sprintf("%03d.csv", i)) 
    }
    csv <- lapply(files, read.csv)
    result <- do.call(rbind, csv)
  }
  lapply(result[result$ID %in% id,][pollutant], mean, na.rm = TRUE)
}
pollutantmean("specdata", "nitrate", id=23)
dput(pollutantmean, "pollutantmean.R")
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate")

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))


complete <- function(directory, id=1:332){
  df <- data.frame(matrix(NA, nrow = length(id), ncol = 2))
  if (length(id) ==1){
    file <- file.path(directory, sprintf("%03d.csv", id))
    result <- read.csv(file)
    nobs <- nrow(result[complete.cases(result),])
    df[1,1] <- id
    df[1,2] <- nobs
    }
  else {
    for (i in 1:length(id)){
      id_num <- id[i]
      file <- file.path(directory, sprintf("%03d.csv", id_num)) 
      result <- read.csv(file)
      nobs<-as.numeric(nrow(result[complete.cases(result),]))
      df[i,1] <- id_num
      df[i,2] <- nobs
    }
  }
  names(df)[1] <- 'id'
  names(df)[2] <- 'nobs'
  df
}


complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)

x<-complete("specdata")
moremil <- x[x$nobs > 1000,]

corr <- function(directory, threshold=0){
  all<-complete("specdata")
  th <- all[all$nobs > threshold,]
  if (nrow(th)==0){ 
    corrs <- vector(mode="numeric", length=0)
  }
  else {
    corrs <- list()
    for (i in 1:nrow(th)){
      id = th$id[i]
      file <- file.path(directory, sprintf("%03d.csv", id)) 
      df <- read.csv(file)
      corrs[i]<-cor(df$sulfate, df$nitrate, use="complete.obs")
    }
    corrs<-unlist(corrs, use.names=FALSE)
  }
  corrs
}

cr <- corr("specdata",5000)
summary(cr)

