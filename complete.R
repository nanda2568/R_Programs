complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  ## NOTE: Accompanying input csv files are in specdata.zip

  l <- length(id)                     # Length of the monitor id objects
  countFrame <- data.frame(id = numeric(l), nobs = numeric(l))   # create data frame to hold the count of observations
  
  for (i in seq_along(id)){           # for all monitor ids (csv files) passed as an argument
    fiName <- formatC(id[i], digits = 3, width = 3, flag = "0")
    fileName <- paste(fiName, "csv", sep=".")
    fullFileName <- paste(directory, fileName, sep="/")
    pollutantData <- read.csv(fullFileName, header = TRUE)[, c("sulfate", "nitrate")]
    count <- 0                        # Number of valid observations
    # Loop through the pollutant column and count all non-NA values
    for(j in 1: nrow(pollutantData)){
      if ((is.na(pollutantData$sulfate[j]) == FALSE) && (is.na(pollutantData$nitrate[j]) == FALSE)){        
        count <- count + 1
      }
    }
    countFrame$id[i] <- id[i]
    countFrame$nobs[i] <- count
  }
  
  return(countFrame)                   # return data frame to display on the console
  
  ## Usage and examples:
  ## source("complete.R")
  ## complete("specdata", 1)
  
  ##   id nobs
  ## 1  1  117
  
  ## complete("specdata", c(2, 4, 8, 10, 12))
  
  ##   id nobs
  ## 1  2 1041
  ## 2  4  474
  ## 3  8  192
  ## 4 10  148
  ## 5 12   96
  
  ## complete("specdata", 30:25)
  
  ##   id nobs
  ## 1 30  932
  ## 2 29  711
  ## 3 28  475
  ## 4 27  338
  ## 5 26  586
  ## 6 25  463
  
  ## complete("specdata", 3)
  
  ##   id nobs
  ## 1  3  243
  
}